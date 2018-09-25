package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder.SimulationStatus
import it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder.SimulationStatus.{EmptySimulation, FullSimulation, SimulationWithAnimals, SimulationWithPlants}
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.{AnimalBuilder, PlantBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, CompleteSimulationBuildException, InvalidParamValueBuildException}

import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._

trait SimulationBuilder[T <: SimulationStatus] extends GenericBuilder[T, FullSimulation, PartialSimulationData, CompleteSimulationData] {
  def addAnimals(animals: Iterable[(AnimalBuilder[_], Int)]): SimulationBuilder[T with SimulationWithAnimals]
  def addPlants(plants: Iterable[(PlantBuilder[_], Int)]): SimulationBuilder[T with SimulationWithPlants]
}

object SimulationBuilder {

  def apply(): SimulationBuilder[EmptySimulation] = new SimulationBuilderImpl[EmptySimulation](Seq(), Seq())

  private class SimulationBuilderImpl[T <: SimulationStatus](animals: Iterable[(AnimalBuilder[_], Int)],
                                                             plants: Iterable[(PlantBuilder[_], Int)])
                                                            (implicit val status: TypeTag[T])
    extends SimulationBuilder[T] with BaseBuildableGenericBuilder[T, FullSimulation, PartialSimulationData, CompleteSimulationData] {

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
          val exc: Option[CompleteBuildException] = check._1 ++: checkProperties()
          if(exc.isEmpty)
            Success(new SimulationDataImpl(check._2, check._3) with CompleteSimulationData)
          else
            Failure(new CompleteSimulationBuildException(exc.get, buildPartialInstance()))
        case _ =>
          Failure(new CompleteSimulationBuildException("Simulation: animals and plants must be set",
            buildPartialInstance()))
      }
    }

    private def checkComplete(): (Option[CompleteBuildException], Iterable[(CompleteAnimalData, Int)], Iterable[(CompletePlantData, Int)]) = {
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

    private def checkProperties(): Option[CompleteBuildException] = {
      var exception: Option[CompleteBuildException] = None
      if(!animals.isValid())
        exception = exception ++: InvalidParamValueBuildException("Simulation" ,"animals", animals)
      if(!plants.isValid())
        exception = exception ++: InvalidParamValueBuildException("Simulation" ,"plants", plants)
      exception
    }

    override protected def buildPartialInstance(): PartialSimulationData = {
      new SimulationDataImpl[PartialAnimalData, PartialPlantData](animals.map(t => (t._1.build(), t._2)),
        plants.map(t => (t._1.build(), t._2)))
    }
  }

  sealed trait SimulationStatus extends BuilderStatus
  object SimulationStatus {
    sealed trait EmptySimulation extends SimulationStatus
    sealed trait SimulationWithPlants extends SimulationStatus
    sealed trait SimulationWithAnimals extends SimulationStatus

    type FullSimulation = EmptySimulation with SimulationWithPlants with SimulationWithAnimals
  }

  private class SimulationDataImpl[A <: PartialAnimalData, P <: PartialPlantData](_animals: Iterable[(A, Int)],
                                                                                  _plants: Iterable[(P, Int)]
                                                                                 ) extends SimulationData[A, P] {
    import BuildersValidationImplicits._
    override val getAnimals: Option[Iterable[(A, Int)]] = _animals.boxToValidOption()
    override val getPlants: Option[Iterable[(P, Int)]] = _plants.boxToValidOption()
  }
}