package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AnimalData.{AnimalDataImpl, CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus.FullAnimal
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

  def apply(): SimulationBuilder[EmptySimulation] = new SimulationBuilderImpl[EmptySimulation](None, None)

  private class SimulationBuilderImpl[T <: SimulationStatus](animals: Option[Iterable[(_ <: PartialAnimalData, Int)]],
                                                             plants: Option[Iterable[(_ <: PartialPlantData, Int)]])
                                                            (implicit val status: TypeTag[T]) extends SimulationBuilder[T] {
    def addAnimals(animals: Iterable[(_ <: PartialAnimalData, Int)]): SimulationBuilder[T with SimulationWithAnimals] = {
      new SimulationBuilderImpl(Some(animals), plants)
    }
    def addPlants(plants: Iterable[(_ <: PartialPlantData, Int)]): SimulationBuilder[T with SimulationWithPlants] = {
      new SimulationBuilderImpl(animals, Some(plants))
    }

    def buildComplete(implicit ev: T =:= FullSimulation): CompleteSimulationData = {
      val check = checkComplete()
      check._1.foreach(throw _)
      new SimulationDataImpl(Some(check._2), Some(check._3)) with FullSimulationData[CompleteAnimalData, CompletePlantData]
    }

    private def checkComplete(): (Option[Exception], Iterable[(CompleteAnimalData, Int)], Iterable[(CompletePlantData, Int)]) ={
      //TODO concat like list Nil :: ecc...
      var exception: Exception = null
      val a: Iterable[(CompleteAnimalData, Int)] = animals.getOrElse(throw new IllegalStateException()).flatMap({
        case t: (CompleteAnimalData, Int) =>
          Some((t._1, t._2))
        case _ =>
          None
      })
      if(animals.get.size != a.size) {
        exception = new IllegalStateException()
      }
      val p: Iterable[(CompletePlantData, Int)] = plants.getOrElse(throw new IllegalStateException()).flatMap({
        case(plant: CompletePlantData, i: Int) =>
          Some((plant, i))
        case _ =>
          None
      })
      if(p.size != plants.get.size)
        exception = new IllegalStateException()
      if(exception == null) {
        //(None, a, p)
        (None, a, p)
      } else {
        (Some(exception), Seq(), Seq())
      }
    }
  }

  private case class SimulationDataImpl[A <: PartialAnimalData, P <: PartialPlantData](
                                                                                        getAnimals: Option[Iterable[(A, Int)]],
                                                                                        getPlants: Option[Iterable[(P, Int)]]
                                                                                      ) extends SimulationData[A, P]

  sealed trait SimulationStatus
  object SimulationStatus {
    sealed trait EmptySimulation extends SimulationStatus
    sealed trait SimulationWithPlants extends SimulationStatus
    sealed trait SimulationWithAnimals extends SimulationStatus

    type FullSimulation = EmptySimulation with SimulationWithPlants with SimulationWithAnimals
  }
}