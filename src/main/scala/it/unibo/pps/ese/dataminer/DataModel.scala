package it.unibo.pps.ese.dataminer

import it.unibo.pps.ese.utils.Point

trait EntityLog {
  val id : EntityId
  val structuralData: StructuralData
  val dynamicData: Seq[(Era, DynamicData)]
}

trait EntityTimedRecord {
  val id: EntityId
  val era: Era
  val structuralData: StructuralData
  val dynamicData: DynamicData
}

trait EntityStaticRecord {
  val id: EntityId
  val data: StructuralData
}

trait EntityDynamicRecord {
  val id: EntityId
  val data: DynamicData
}

trait DynamicData {
  val position: Point
  val nutritionalValue: Double
}

trait AnimalDynamicData extends DynamicData {
  val age: Integer
  val energy: Double
  val lifePhase: LifePhase
  val speed: Double
}

trait PlantDynamicData extends DynamicData

trait StructuralData extends {
  val species: Species
  val reign: Reign
  val gender: Sex
  val diet: Diet
  val height: Double
  val defense: Double
}

trait AnimalStructuralData extends StructuralData {
  val strength: Double
  val actionField: Double
  val visualField: Double
  val averageLife: Double
  val energyRequirements: Double
  val maturity: Double
  val oldness: Double
  val decay: Double
  val speed: Double
  val fertility: Double
}

trait PlantStructuralData extends StructuralData {
  val availability: Double
}