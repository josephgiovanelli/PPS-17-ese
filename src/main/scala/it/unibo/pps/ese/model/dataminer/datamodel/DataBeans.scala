package it.unibo.pps.ese.model.dataminer.datamodel

import it.unibo.pps.ese.model.components.animals.brain.ActionTypes
import it.unibo.pps.ese.model.dataminer.DataModelSupport._
import it.unibo.pps.ese.utils.Point

case class EntityLogImpl(id: EntityId,
                         structuralData: StructuralData,
                         dynamicData: Seq[(Era, DynamicData)]) extends EntityLog

case class EntityTimedRecordImpl(id: EntityId,
                                 era: Era,
                                 structuralData: StructuralData,
                                 dynamicData: DynamicData) extends EntityTimedRecord

case class EntityStaticRecordImpl(id: EntityId, data: StructuralData) extends EntityStaticRecord

case class EntityDynamicRecordImpl(id: EntityId, data: DynamicData) extends EntityDynamicRecord

case class AnimalStructuralDataImpl(strength: Double,
                                    actionField: Double,
                                    visualField: Double,
                                    averageLife: Double,
                                    energyRequirements: Double,
                                    maturity: Double,
                                    oldness: Double,
                                    decay: Double,
                                    speed: Double,
                                    fertility: Double,
                                    species: Species,
                                    reign: Reign,
                                    gender: Sex,
                                    diet: Diet,
                                    height: Double,
                                    defense: Double) extends AnimalStructuralData

case class PlantStructuralDataImpl(species: Species,
                                   reign: Reign,
                                   gender: Sex,
                                   diet: Diet,
                                   height: Double,
                                   defense: Double) extends PlantStructuralData

case class AnimalDynamicDataImpl(age: Integer,
                                 energy: Double,
                                 lifePhase: LifePhase,
                                 speed: Double,
                                 fertility: Double,
                                 position: Point,
                                 nutritionalValue: Double,
                                 coupling: Seq[String],
                                 eating: Seq[String],
                                 givingBirth: Seq[String],
                                 producedMutantGenes: Seq[String],
                                 will: ActionTypes) extends AnimalDynamicData

case class PlantDynamicDataImpl(position: Point,
                                nutritionalValue: Double) extends PlantDynamicData
