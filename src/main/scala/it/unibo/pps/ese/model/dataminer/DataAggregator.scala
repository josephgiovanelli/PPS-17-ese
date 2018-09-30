package it.unibo.pps.ese.model.dataminer

import it.unibo.pps.ese.controller.simulation.runner.core.data.{EntityInfo, EntityState, ReadOnlyEntityState}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.dataminer.DataModelSupport.Era
import it.unibo.pps.ese.model.dataminer.datamodel._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/**
  * This utility class offers methods for ingest and historicise data from the simulation state
  * @param realTimeState The simulation data source
  */
class DataAggregator(realTimeState: ReadOnlyEntityState) {

  private val _entityDataRepository = EntityDataRepository()

  /**
    * Update the consolidated state using the data from real time state
    * @param era The era to which the data refers
    * @param executionContext An execution context, required for async tasks
    */
  def ingestData(era: Era)(implicit executionContext: ExecutionContext): Unit = {

    @tailrec
    def _ingestData(era: Era, data: Seq[EntityState]): Unit = {
      if (data isEmpty) return
      if (!(_entityDataRepository exists ((data head) entityId))) _entityDataRepository saveStaticEntityData (data head)
      _entityDataRepository saveDynamicEntityData (era, data head)
      _ingestData(era, data tail)
    }

    _ingestData(era, realTimeState getFilteredState(_ => true))
    Future {
      _entityDataRepository generateNewDataNotification era
    }
  }

  /**
    * Get the ingested data repository
    * @return A read-only copy of the consolidated data repository
    */
  def ingestedData: ReadOnlyEntityRepository = _entityDataRepository

  /**
    * Methods used to give a structured form to the unstructured real time data
    */

  private def mapToAnimalStructuralData(state: EntityInfo): StructuralData =
    AnimalStructuralDataImpl(
      state strength,
      state actionField,
      state visualField,
      state averageLife,
      state energyRequirements,
      state endChildPhase,
      state endAdultPhase,
      state percentageDecay,
      state speed,
      state fertility,
      state.species.toString,
      state.reign.toString,
      state.gender.toString,
      state.diet.toString,
      state height,
      state defense
    )

  private def mapToPlantStructuralData(state: EntityInfo): StructuralData =
    PlantStructuralDataImpl(
      state.species.toString,
      state.reign.toString,
      state.gender.toString,
      state.diet.toString,
      state height,
      state defense
    )

  private def mapToAnimalDynamicData(state: EntityInfo): DynamicData =
    AnimalDynamicDataImpl(
      state age,
      state energy,
      state.lifePhase.toString,
      state actualSpeed,
      state actualFertility,
      state position,
      state nutritionalValue,
      state couple,
      state eat,
      state create,
      state genes,
      state will
    )

  private def mapToPlantDynamicData(state: EntityInfo): DynamicData =
    PlantDynamicDataImpl(
      state position,
      state nutritionalValue
    )

  private def mapToStructuralData(state: EntityInfo): StructuralData = {
    if (state.reign == ReignType.ANIMAL) mapToAnimalStructuralData(state)
    else mapToPlantStructuralData(state)
  }

  private def mapToDynamicData(state: EntityInfo): DynamicData = {
    if (state.reign == ReignType.ANIMAL) mapToAnimalDynamicData(state)
    else mapToPlantDynamicData(state)
  }

  private implicit def mapToStaticRecord(state: EntityState): EntityStaticRecord = {
    EntityStaticRecordImpl(state entityId, mapToStructuralData(state.state))
  }

  private implicit def mapToDynamicRecord(state: EntityState): EntityDynamicRecord = {
    EntityDynamicRecordImpl(state entityId, mapToDynamicData(state state))
  }
}
