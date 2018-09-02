package it.unibo.pps.ese.genericworld.model

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.entitybehaviors._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, DietType, PlantInfo, Reign}
import it.unibo.pps.ese.genetics.entities.QualityType.{Attractiveness, _}
import it.unibo.pps.ese.utils.Point

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object ReignType extends Enumeration {
  val ANIMAL, PLANT = Value
}

object WorldBuilder {

  def buildWorldFromSimulationData(simulationConfigPath: String, height: Int, width: Int): World = {

    StaticRules.instance().addSpecies(Set("Gatto", "Giraffa", "ErbaGatta"))
    val worldRules: WorldRulesImpl = decisionsupport.WorldRulesImpl.WorldRulesImpl(Integer.MIN_VALUE, (0, Integer.MAX_VALUE), 0,
      Set(("Gatto", "Giraffa"), ("Giraffa", "ErbaGatta")),
      Set(("Gatto", "Gatto"), ("Giraffa", "Giraffa")))
    StaticRules.instance().setRules(worldRules)

    val world = World(width, height)

    val data = YamlLoader.loadSimulation(simulationConfigPath)
    val initializedSimulation = GeneticsSimulator.beginSimulation(data)

    GeneticsSimulator.speciesList
      .flatMap(x => initializedSimulation.getAllAnimals(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllAnimals.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2))
      .foreach(world addEntity)

    GeneticsSimulator.plantSpeciesList
      .flatMap(x => initializedSimulation.getAllPlant(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllPlant.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2))
      .foreach(world addEntity)

    Await.result(world.requireInfoUpdate, Duration.Inf)
    world
  }

  def initializeEntity(animalInfo: AnimalInfo, position: Point) : Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, animalInfo, position)
    entity addComponent initializeBrainComponent(entity, animalInfo)
    entity addComponent initializePhysicalComponent(entity, animalInfo)
    entity addComponent initializeReproductionComponent(entity, animalInfo)
    entity
  }


  private def initializeEntity(plantInfo: PlantInfo, position: Point) : Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, plantInfo, position)
    entity addComponent initializePhysicalComponent(entity, plantInfo)
    entity
  }

  private def initializeBrainComponent(entity: Entity, animalInfo: AnimalInfo) : Component = {

    implicit def convertKind(dietType: DietType): String = if (dietType.dietName == "H") "herbivore" else "carnivorous"

    BrainComponent(entity specifications,
      500,
      500,
      animalInfo.qualities(Strength).qualityValue,
      animalInfo.qualities(RangeOfAction).qualityValue,
      animalInfo.qualities(FieldOfView).qualityValue,
      animalInfo.qualities(Attractiveness).qualityValue)
  }

  private def initializePhysicalComponent(entity: Entity, animalInfo: AnimalInfo) : Component = {
    PhysicalStatusComponent(
      entity specifications,
      animalInfo.qualities(Life).qualityValue.toInt,
      animalInfo.qualities(EnergyRequirements).qualityValue,
      animalInfo.qualities(Maturity).qualityValue,
      animalInfo.qualities(Oldness).qualityValue,
      animalInfo.qualities(Decline).qualityValue,
      animalInfo.qualities(Speed).qualityValue,
      animalInfo.qualities(Fertility).qualityValue)
  }

  private def initializeBaseInfoComponent(entity: Entity, entityInfo: it.unibo.pps.ese.genetics.entities.EntityInfo, position: Point) : Component = {

    implicit def convertReign(reign: Reign): ReignType.Value = if (reign.reignName == "A") ReignType.ANIMAL else ReignType.PLANT
    implicit def convertDefense(entityInfo: it.unibo.pps.ese.genetics.entities.EntityInfo): Double =
      if (convertReign(entityInfo.species.reign) == ReignType.ANIMAL) entityInfo.qualities(ResistenceToAttack).qualityValue
      else entityInfo.qualities(Hardness).qualityValue

    BaseInfoComponent(
      entity specifications,
      entityInfo.species.name,
      entityInfo.species.reign,
      entityInfo.gender.toString.toLowerCase,
      position,
      entityInfo.qualities(Height).qualityValue,
      entityInfo.qualities(NutritionalValue).qualityValue,
      entityInfo
    )
  }

  private def initializePhysicalComponent(entity: Entity, plantInfo: PlantInfo) : Component = {
    PlantPhysicalComponent(
      entity specifications,
      plantInfo.qualities(Availability).qualityValue)
  }

  def initializeReproductionComponent(entity: Entity, animalInfo: AnimalInfo): Component = {
    ReproductionComponent(
      entity.specifications,
      //TODO exception, key not exists
      //animalInfo.qualities(Fecundity).qualityValue,
      3,
      GeneticsSimulator,
      animalInfo.genome,
      //TODO
      10,
      7,
      0.05,
      animalInfo.qualities(EnergyRequirements).qualityValue
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
