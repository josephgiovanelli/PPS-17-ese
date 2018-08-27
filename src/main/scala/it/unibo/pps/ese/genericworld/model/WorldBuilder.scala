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

    val world = World(width, height)

    val data = new YamlLoader().loadSimulation(simulationConfigPath)
    val geneticsSimulator = GeneticsSimulator
    val initializedSimulation = geneticsSimulator.beginSimulation(data)

    geneticsSimulator.speciesList
      .flatMap(x => initializedSimulation.getAllAnimals(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllAnimals.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2))
      .foreach(world addEntity)

    Await.result(world.requireInfoUpdate, Duration.Inf)
    world
  }

  private def initializeEntity(animalInfo: AnimalInfo, position: Point) : Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, animalInfo)
    entity addComponent initializeBrainComponent(entity, animalInfo, position)
    entity addComponent initializePhysicalComponent(entity, animalInfo)
    entity
  }

  private def initializeBrainComponent(entity: Entity, animalInfo: AnimalInfo, position: Point) : Component = {

    implicit def convertKind(dietType: DietType): String = if (dietType.dietName == "H") "herbivore" else "carnivorous"

    BrainComponent(entity specifications,
      100,
      100,
      position,
      animalInfo.qualities(Height).qualityValue,
      animalInfo.qualities(Strength).qualityValue,
      animalInfo.qualities(ResistenceToAttack).qualityValue,
      animalInfo.dietType,
      animalInfo.qualities(RangeOfAction).qualityValue,
      animalInfo.qualities(FieldOfView).qualityValue,
      animalInfo.qualities(Attractiveness).qualityValue,
      animalInfo.gender.toString.toLowerCase)
  }

  private def initializePhysicalComponent(entity: Entity, animalInfo: AnimalInfo) : Component = {
    PhysicalStatusComponent(
      entity specifications,
      animalInfo.qualities(Life).qualityValue.toInt,
      animalInfo.qualities(EnergyRequirements).qualityValue,
      animalInfo.qualities(NutritionalValue).qualityValue,
      animalInfo.qualities(Maturity).qualityValue,
      animalInfo.qualities(Oldness).qualityValue,
      animalInfo.qualities(Decline).qualityValue,
      animalInfo.qualities(Speed).qualityValue,
      animalInfo.qualities(Fertility).qualityValue)
  }

  private def initializeBaseInfoComponent(entity: Entity, animalInfo: AnimalInfo) : Component = {
    BaseInfoComponent(
      entity specifications,
      animalInfo.species.name,
      animalInfo.species.reign.reignName
    )
  }

  private def distinctRandomPoints(n:Int, x:Int, y:Int):Set[Point] = {
    import scala.util.Random
    require(n < x * y)
    Stream.continually((Random.nextInt(x), Random.nextInt(y))).scanLeft(Set[Point]()) {
      (accumulator, el) => accumulator + Point(el._1, el._2)
    }.dropWhile(_.size < n).head
  }
}
