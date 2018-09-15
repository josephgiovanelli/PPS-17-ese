package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.loader.data.builder.SimulationBuilder.SimulationStatus
import it.unibo.pps.ese.controller.loader.data.builder.SimulationBuilder.SimulationStatus.{EmptySimulation, FullSimulation, SimulationWithAnimals, SimulationWithPlants}
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException

import scala.reflect.runtime.universe._

trait SimulationBuilder[T <: SimulationStatus] {
  def addAnimals(animals: Iterable[(_ <: PartialAnimalData, Int)]): SimulationBuilder[T with SimulationWithAnimals]
  def addPlants(plants: Iterable[(_ <: PartialPlantData, Int)]): SimulationBuilder[T with SimulationWithPlants]
  def buildComplete(implicit ev: T =:= FullSimulation): CompleteSimulationData
  def build(): SimulationData[_ <: PartialAnimalData, _ <: PartialPlantData]
}

object SimulationBuilder {

  def apply(): SimulationBuilder[EmptySimulation] = new SimulationBuilderImpl[EmptySimulation](Seq(), Seq())

  private class SimulationBuilderImpl[T <: SimulationStatus](animals: Iterable[(_ <: PartialAnimalData, Int)],
                                                             plants: Iterable[(_ <: PartialPlantData, Int)])
                                                            (implicit val status: TypeTag[T]) extends SimulationBuilder[T] {
    def addAnimals(animals: Iterable[(_ <: PartialAnimalData, Int)]): SimulationBuilder[T with SimulationWithAnimals] = {
      new SimulationBuilderImpl(animals, plants)
    }
    def addPlants(plants: Iterable[(_ <: PartialPlantData, Int)]): SimulationBuilder[T with SimulationWithPlants] = {
      new SimulationBuilderImpl(animals, plants)
    }

    def buildComplete(implicit ev: T =:= FullSimulation): CompleteSimulationData = {
      val check = checkComplete()
      check._1.foreach(throw _)
      new SimulationDataImpl(check._2, check._3) with CompleteSimulationData
    }

    def build(): SimulationData[_ <: PartialAnimalData, _ <: PartialPlantData] = {
      //require(status.tpe <:< st.tpe)
      status.tpe match {
        case t if t <:< typeOf[FullSimulation] =>
          val check = checkComplete()
          if(check._1.isEmpty)
            new SimulationDataImpl(check._2, check._3) with CompleteSimulationData
          else
            new SimulationDataImpl[PartialAnimalData, PartialPlantData](animals, plants)
        case _ =>
          new SimulationDataImpl[PartialAnimalData, PartialPlantData](animals, plants)
      }
    }

    private def matchTuple[A <: PartialAnimalData](tuple2: (A, Int))(implicit aType: TypeTag[A]): Option[(CompleteAnimalData, Int)] = tuple2 match {
      case (animal: CompleteAnimalData, quantity) =>
        Some((animal, quantity))
      case _ =>
        None
    }

    private def checkComplete(): (Option[Exception], Iterable[(CompleteAnimalData, Int)], Iterable[(CompletePlantData, Int)]) ={
      var exception: Option[CompleteBuildException] = None
      val a: Iterable[(CompleteAnimalData, Int)] = animals.flatMap(matchTuple(_))
      if(animals.size != a.size) {
        exception = exception ++: new CompleteBuildException("All animals must be complete")
      }
      val p: Iterable[(CompletePlantData, Int)] = plants.flatMap({
        case(plant: CompletePlantData, i: Int) =>
          Some((plant, i))
        case _ =>
          None
      })
      if(p.size != plants.size)
        exception = exception ++: new CompleteBuildException("All plants must be complete")
      (exception, a, p)
    }
  }

  sealed trait SimulationStatus
  object SimulationStatus {
    sealed trait EmptySimulation extends SimulationStatus
    sealed trait SimulationWithPlants extends SimulationStatus
    sealed trait SimulationWithAnimals extends SimulationStatus

    type FullSimulation = EmptySimulation with SimulationWithPlants with SimulationWithAnimals
  }

  private class SimulationDataImpl[A <: PartialAnimalData, P <: PartialPlantData](_animals: Iterable[(A, Int)],
                                                                                  _plants: Iterable[(P, Int)]
                                                                                 ) extends SimulationData[A, P] {
    override val getAnimals: Option[Iterable[(A, Int)]] = if(_animals.isEmpty) None else Some(_animals)
    override val getPlants: Option[Iterable[(P, Int)]] = if(_plants.isEmpty) None else Some(_plants)
  }
}