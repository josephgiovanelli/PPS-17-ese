package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.loader.data.builder.SimulationBuilder.SimulationStatus
import it.unibo.pps.ese.controller.loader.data.builder.SimulationBuilder.SimulationStatus.{EmptySimulation, FullSimulation, SimulationWithAnimals, SimulationWithPlants}
import it.unibo.pps.ese.controller.loader.data._

import scala.reflect.runtime.universe._

trait SimulationBuilder[T <: SimulationStatus] {
  def addAnimals(animals: Iterable[(_ <: PartialAnimalData, Int)]): SimulationBuilder[T with SimulationWithAnimals]
  def addPlants(plants: Iterable[(_ <: PartialPlantData, Int)]): SimulationBuilder[T with SimulationWithPlants]
  def buildComplete(implicit ev: T =:= FullSimulation): CompleteSimulationData
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
      new SimulationDataImpl(check._2, check._3) with FullSimulationData[CompleteAnimalData, CompletePlantData]
    }

    private def checkComplete(): (Option[Exception], Iterable[(CompleteAnimalData, Int)], Iterable[(CompletePlantData, Int)]) ={
      //TODO concat like list Nil :: ecc...
      var exception: Exception = null
      val a: Iterable[(CompleteAnimalData, Int)] = animals.flatMap({
        case t: (CompleteAnimalData, Int) =>
          Some((t._1, t._2))
        case _ =>
          None
      })
      if(animals.size != a.size) {
        exception = new IllegalStateException()
      }
      val p: Iterable[(CompletePlantData, Int)] = plants.flatMap({
        case(plant: CompletePlantData, i: Int) =>
          Some((plant, i))
        case _ =>
          None
      })
      if(p.size != plants.size)
        exception = new IllegalStateException()
      if(exception == null) {
        //(None, a, p)
        (None, a, p)
      } else {
        (Some(exception), Seq(), Seq())
      }
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