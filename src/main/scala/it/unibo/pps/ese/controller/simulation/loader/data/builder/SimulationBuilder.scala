package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.simulation.loader.data.EntityData.CompleteEntityData
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder.SimulationStatus
import it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder.SimulationStatus.{EmptySimulation, FullSimulation, SimulationWithAnimals, SimulationWithPlants}
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.{AnimalBuilder, EntityBuilder, PlantBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, CompleteSimulationBuildException, InvalidParamValueBuildException}

import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._

/** Builder that can build a PartialSimulationData as partial data instance and a CompleteSimulationData as complete data
  * instance
  *
  * @tparam T Builder's current status
  */
trait SimulationBuilder[T <: SimulationStatus] extends GenericBuilder[T, FullSimulation, PartialSimulationData, CompleteSimulationData] {
  /** Add animals to builder
    *
    * @param animals Animals' builders collection
    * @return New builder with updated param and status
    */
  def addAnimals(animals: Iterable[(AnimalBuilder[_], Int)]): SimulationBuilder[T with SimulationWithAnimals]

  /** Add plants to builder
    *
    * @param plants Plants' builders collection
    * @return New builder with updated param and status
    */
  def addPlants(plants: Iterable[(PlantBuilder[_], Int)]): SimulationBuilder[T with SimulationWithPlants]
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder]]*/
object SimulationBuilder {

  /** Create a new empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder]]
    *
    * @return An empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder]]
    */
  def apply(): SimulationBuilder[EmptySimulation] = new SimulationBuilderImpl[EmptySimulation](Seq(), Seq())

  private class SimulationBuilderImpl[T <: SimulationStatus](_animals: Iterable[(AnimalBuilder[_], Int)],
                                                             _plants: Iterable[(PlantBuilder[_], Int)])
                                                            (implicit val status: TypeTag[T])
    extends SimulationBuilder[T] with BaseGenericBuilder[T, FullSimulation, PartialSimulationData, CompleteSimulationData] {

    def addAnimals(animals: Iterable[(AnimalBuilder[_], Int)]): SimulationBuilder[T with SimulationWithAnimals] = {
      new SimulationBuilderImpl(_animals ++ animals, _plants)
    }
    def addPlants(plants: Iterable[(PlantBuilder[_], Int)]): SimulationBuilder[T with SimulationWithPlants] = {
      new SimulationBuilderImpl(_animals, _plants ++ plants)
    }

    override def tryCompleteBuild(): Try[CompleteSimulationData] = {
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

    /** Check builder status depending by internal builders
      *
      * @return Optional exception occurred during checks and collections containing complete builded entities
      */
    def checkComplete(): (Option[CompleteBuildException], Iterable[(CompleteAnimalData, Int)], Iterable[(CompletePlantData, Int)]) = {
      var exception: Option[CompleteBuildException] = None
      val checkA = checkEntities(_animals, "animals")
      exception = exception ++: checkA._1
      val a = checkA._2.asInstanceOf[Iterable[(CompleteAnimalData, Int)]]
      val checkP = checkEntities(_plants, "plants")
      exception = exception ++: checkP._1
      val p = checkP._2.asInstanceOf[Iterable[(CompletePlantData, Int)]]
      (exception, a, p)
    }

    /** Check validity of entities' builder collection
      *
      * @param entities Entities to check
      * @param name Entities' name to return good exception
      * @return Optional exception occurred during checks and collection containing complete builded entities
      */
    def checkEntities(entities: Iterable[(EntityBuilder[_], Int)], name: String): (Option[CompleteBuildException], Iterable[(CompleteEntityData, Int)]) = {
      /*
       * Check if all entities are complete building them
       */
      var exception: Option[CompleteBuildException] = None
      val tries: Iterable[(Try[CompleteEntityData], Int)] = entities.map(t => (t._1.tryCompleteBuild(), t._2))
      val complete: Iterable[(CompleteEntityData, Int)] = tries.collect({
        case (Success(value), i) =>
          (value, i)
      })
      if(entities.size != complete.size) {
        exception = exception ++: CompleteBuildException("Simulation: All " + name + " must be complete",
          tries.collect({case (Failure(exc: CompleteBuildException), _) => exc}))
      }
      if(complete.size != complete.toMap.size) {
        exception = exception ++: CompleteBuildException("Simulation: there are duplicated " + name)
      }
      if(!complete.map(_._2).forall(_ >= 0)) {
        exception = exception ++: CompleteBuildException("Simulation: all " + name + "must have 0 or greater quantity")
      }
      (exception, complete)
    }

    /** Check builder status depending by fields value
      *
      * @return Optional exception occurred during checks
      */
    private def checkProperties(): Option[CompleteBuildException] = {
      var exception: Option[CompleteBuildException] = None
      if(!_animals.isValid())
        exception = exception ++: InvalidParamValueBuildException("Simulation" ,"animals", _animals)
      if(!_plants.isValid())
        exception = exception ++: InvalidParamValueBuildException("Simulation" ,"plants", _plants)
      exception
    }

    override protected def buildPartialInstance(): PartialSimulationData = {
      new SimulationDataImpl[PartialAnimalData, PartialPlantData](_animals.map(t => (t._1.build(), t._2)),
        _plants.map(t => (t._1.build(), t._2)))
    }
  }

  /** Interface that represent generic simulation's builder status*/
  sealed trait SimulationStatus extends BuilderStatus
  /** Object containing all possible simulation's builder statuses*/
  object SimulationStatus {
    sealed trait EmptySimulation extends SimulationStatus
    sealed trait SimulationWithPlants extends SimulationStatus
    sealed trait SimulationWithAnimals extends SimulationStatus

    /**Type that defines complete simulation's builder status*/
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