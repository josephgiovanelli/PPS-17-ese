package it.unibo.pps.ese.genericworld.model

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.entitybehaviors.{BrainComponent, PhysicalStatusComponent, StaticRules, decisionsupport}
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, DietType}
import it.unibo.pps.ese.genetics.entities.QualityType._
import it.unibo.pps.ese.utils.Point

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object WorldBuilder {

  def buildWorldFromSimulationData(simulationConfigPath: String, height: Int, width: Int): World = {

    StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
    val worldRules: WorldRulesImpl = decisionsupport.WorldRulesImpl.WorldRulesImpl(3, (0, 5), 3, Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
      Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))
    StaticRules.instance().setRules(worldRules)

    val world = World()

    val data = new YamlLoader().loadSimulation(simulationConfigPath)
    val geneticsSimulator = GeneticsSimulator
    val initializedSimulation = geneticsSimulator.beginSimulation(data)

    geneticsSimulator.speciesList
      .flatMap(x => initializedSimulation.getAllAnimals(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllAnimals.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2))
      .foreach(world addEntity)

    Await.result(world.requireInfoUpdate, 1 second)
    world
  }

  private def initializeEntity(animalInfo: AnimalInfo, position: Point) : Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeBrainComponent(entity, animalInfo, position)
    entity addComponent initializePhysicalComponent(entity, animalInfo)
    entity
  }

  private def initializeBrainComponent(entity: Entity, animalInfo: AnimalInfo, position: Point) : Component = {

    implicit def convertKind(dietType: DietType): String = if (dietType.dietName == "H") "herbivore" else "carnivorous"

    BrainComponent(entity specifications,
      position,
      animalInfo.animalQualities(Height).qualityValue.toInt,
      animalInfo.animalQualities(Strength).qualityValue.toInt,
      animalInfo.animalQualities(ResistenceToAttack).qualityValue.toInt,
      animalInfo.dietType,
      animalInfo.animalQualities(RangeOfAction).qualityValue.toInt,
      animalInfo.animalQualities(FieldOfView).qualityValue.toInt,
      animalInfo.animalQualities(Attractiveness).qualityValue.toInt,
      animalInfo.gender.toString.toLowerCase)
  }

  private def initializePhysicalComponent(entity: Entity, animalInfo: AnimalInfo) : Component = {
    PhysicalStatusComponent(
      entity specifications,
      animalInfo.animalQualities(Life).qualityValue.toInt,
      animalInfo.animalQualities(EnergyRequirements).qualityValue.toInt,
      animalInfo.animalQualities(NutritionalValue).qualityValue.toInt,
      animalInfo.animalQualities(Maturity).qualityValue.toInt,
      animalInfo.animalQualities(Oldness).qualityValue.toInt,
      animalInfo.animalQualities(Decline).qualityValue,
      animalInfo.animalQualities(Speed).qualityValue.toInt,
      animalInfo.animalQualities(Fertility).qualityValue.toInt)
  }

  private def distinctRandomPoints(n:Int, x:Int, y:Int):Set[Point] = {
    import scala.util.Random
    require(n < x * y)
    Stream.continually((Random.nextInt(x), Random.nextInt(y))).scanLeft(Set[Point]()) {
      (accum, el) => accum + Point(el._1, el._2)
    }.dropWhile(_.size < n).head
  }
}
