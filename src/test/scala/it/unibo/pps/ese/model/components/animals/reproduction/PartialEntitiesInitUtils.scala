package it.unibo.pps.ese.model.components.animals.reproduction

import it.unibo.pps.ese.controller.simulation.runner.core.{Component, Entity}
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.entities.{AnimalInfo, Female, Male, Quality}
import it.unibo.pps.ese.model.genetics.entities.QualityType.{EnergyRequirements, Fecundity}
import it.unibo.pps.ese.utils.Point

import scala.concurrent.ExecutionContext

object PartialEntitiesInitUtils {
  def entityInit(animalInfo: AnimalInfo, position: Point, fertility: Double, active: Option[String] = None)(implicit i: Iterator[Int], executionContext: ExecutionContext): Entity = {
    var gender = ""
    if(animalInfo.gender == Male) {
      gender = "male"
    } else if(animalInfo.gender == Female) {
      gender = "female"
    }
    behaviourEntityInit(
      baseEntityInit(animalInfo),
      animalInfo,
      position,
      gender,
      fertility,
      active)
  }

  def baseEntityInit(animalInfo: AnimalInfo)(implicit i: Iterator[Int], executionContext: ExecutionContext) : Entity = {
    val entity = Entity("improved" + i.next())
    entity addComponent initializeReproductionComponent(entity, animalInfo)
    entity
  }

  def behaviourEntityInit(entity: Entity, info: AnimalInfo, position: Point, gender: String, fertility: Double, active: Option[String])(implicit executionContext: ExecutionContext): Entity = {
    entity addComponent FakeComponent(entity.specifications, info.species.name, gender, position, fertility, active)
    entity
  }

  def initializeReproductionComponent(entity: Entity, info: AnimalInfo)(implicit i: Iterator[Int], executionContext: ExecutionContext): Component = {
    def animalCreationFunction: (AnimalInfo, Point) => Entity = (a, p) => entityInit(a, p, 0.0)
    ReproductionComponent(
      entity.specifications,
      info.qualities.getOrElse(Fecundity, Quality(0, Fecundity)).qualityValue,
      GeneticsSimulator,
      info.genome,
      0.5,
      6,
      -1,
      info.qualities(EnergyRequirements).qualityValue,
      animalCreationFunction
    )
  }
}
