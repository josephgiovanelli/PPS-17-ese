package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder.SimulationStatus
import it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder.SimulationStatus.{EmptySimulation, FullSimulation, SimulationWithAnimals, SimulationWithPlants}
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.{AnimalBuilder, PlantBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, CompleteSimulationBuildException}

import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._

trait SimulationBuilder[T <: SimulationStatus] {
  def addAnimals(animals: Iterable[(AnimalBuilder[_], Int)]): SimulationBuilder[T with SimulationWithAnimals]
  def addPlants(plants: Iterable[(PlantBuilder[_], Int)]): SimulationBuilder[T with SimulationWithPlants]
  def buildComplete(implicit ev: T =:= FullSimulation): CompleteSimulationData
  def tryCompleteBuild: Try[CompleteSimulationData]
  def build(): SimulationData[_ <: PartialAnimalData, _ <: PartialPlantData]
}

object SimulationBuilder {

  def apply(): SimulationBuilder[EmptySimulation] = new SimulationBuilderImpl[EmptySimulation](Seq(), Seq())

  private class SimulationBuilderImpl[T <: SimulationStatus](animals: Iterable[(AnimalBuilder[_], Int)],
                                                             plants: Iterable[(PlantBuilder[_], Int)])
                                                            (implicit val status: TypeTag[T]) extends SimulationBuilder[T] {
    def addAnimals(animals: Iterable[(AnimalBuilder[_], Int)]): SimulationBuilder[T with SimulationWithAnimals] = {
      new SimulationBuilderImpl(animals, plants)
    }
    def addPlants(plants: Iterable[(PlantBuilder[_], Int)]): SimulationBuilder[T with SimulationWithPlants] = {
      new SimulationBuilderImpl(animals, plants)
    }

    override def tryCompleteBuild: Try[CompleteSimulationData] = {
      status.tpe match {
        case t if t <:< typeOf[FullSimulation] =>
          val check = checkComplete()
          if(check._1.isEmpty)
            Success(new SimulationDataImpl(check._2, check._3) with CompleteSimulationData)
          else
            Failure(new CompleteSimulationBuildException(check._1.get, new SimulationDataImpl[PartialAnimalData, PartialPlantData](
              animals.map(t => (t._1.build(), t._2)), plants.map(t => (t._1.build(), t._2)))))
        case _ =>
          Failure(new CompleteSimulationBuildException("Simulation: animals and plants must be set",
            new SimulationDataImpl[PartialAnimalData, PartialPlantData](animals.map(t => (t._1.build(), t._2)),
              plants.map(t => (t._1.build(), t._2)))))
      }
    }

    def buildComplete(implicit ev: T =:= FullSimulation): CompleteSimulationData = {
      tryCompleteBuild match {
        case Success(value) =>
          value
        case Failure(exception) =>
          throw exception
      }
    }

    def build(): SimulationData[_ <: PartialAnimalData, _ <: PartialPlantData] = {
      //require(status.tpe <:< st.tpe)
      tryCompleteBuild match {
        case Success(value) =>
          value
        case Failure(exception: CompleteSimulationBuildException) =>
          exception.partialSimulationData
      }
    }

    private def checkComplete(): (Option[CompleteBuildException], Iterable[(CompleteAnimalData, Int)], Iterable[(CompletePlantData, Int)]) ={
      var exception: Option[CompleteBuildException] = None
      val aTries: Iterable[(Try[CompleteAnimalData], Int)] = animals.map(t => (t._1.tryCompleteBuild(), t._2))
      val a: Iterable[(CompleteAnimalData, Int)] = aTries.collect({
        case (Success(value), i) =>
          (value, i)
      })
      if(animals.size != a.size) {
        exception = exception ++: CompleteBuildException("Simulation: All animals must be complete",
          aTries.collect({case (Failure(exc: CompleteBuildException), _) => exc}))
      }
      val pTries: Iterable[(Try[CompletePlantData], Int)] = plants.map(t => (t._1.tryCompleteBuild, t._2))
      val p: Iterable[(CompletePlantData, Int)] = pTries.collect({
        case (Success(value), i) =>
          (value, i)
      })
      if(plants.size != p.size) {
        exception = exception ++: CompleteBuildException("Simulation: All plants must be complete",
          pTries.collect({case (Failure(exc: CompleteBuildException), _) => exc}))
      }
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