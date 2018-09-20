package it.unibo.pps.ese.genericworld.model

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.loader.data.CompletePlantData
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.dataminer.DataAggregator
import it.unibo.pps.ese.entitybehaviors._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._
import it.unibo.pps.ese.genericworld.controller.{Controller, SimulationController, SimulationLoop}
import it.unibo.pps.ese.genericworld.model.UpdatableWorld.UpdatePolicy.{Deterministic, Stochastic}
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, DietType, PlantInfo, Quality, Reign}
import it.unibo.pps.ese.genetics.entities.QualityType.{Attractiveness, _}
import it.unibo.pps.ese.utils.Point

import scala.collection.immutable
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

object ReignType extends Enumeration {
  val ANIMAL, PLANT = Value
}

object SimulationBuilder {

  sealed trait Simulation
  object Simulation {
    sealed trait EmptySimulation extends Simulation
    sealed trait Dimension extends Simulation
    sealed trait Data extends Simulation

    type ReadySimulation = EmptySimulation with Dimension with Data
  }
}

class SimulationBuilder[Simulation <: SimulationBuilder.Simulation]
(width: Int = 0, height: Int = 0, data: CompleteSimulationData = null)(implicit executionContext: ExecutionContext) {

  import SimulationBuilder.Simulation._

  def dimension(width: Int, height: Int): SimulationBuilder[Simulation with Dimension] =
    new SimulationBuilder(width, height, data)

  def data(data: CompleteSimulationData): SimulationBuilder[Simulation with Data] =
    new SimulationBuilder(width, height, data)

  def build(implicit ev: Simulation =:= ReadySimulation): SimulationController = controller

  val attackThreshold = 10
  val heighThreshold = 7
  val couplingThreshold = 6
  private lazy val controller: SimulationController = {

    import EntityBuilderHelpers._

    val world = World[Stochastic](width, height)

    val geneticsSimulator = GeneticsSimulator
    val initializedSimulation = geneticsSimulator.beginSimulation(data)
    def animalCreationFunction: (AnimalInfo, Point) => Entity =
      (a, p) => EntityBuilderHelpers.initializeEntity(a, p, width, height, animalCreationFunction)

    StaticRules.instance().addSpecies((geneticsSimulator.speciesList ++ geneticsSimulator.plantSpeciesList) toSet)
    val worldRules: WorldRulesImpl = decisionsupport.WorldRulesImpl.WorldRulesImpl(attackThreshold,heighThreshold,couplingThreshold)
    StaticRules.instance().setRules(worldRules)

    geneticsSimulator.speciesList
      .flatMap(x => initializedSimulation.getAllAnimals(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllAnimals.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2, height, width, animalCreationFunction))
      .foreach(world addEntity)

    geneticsSimulator.plantSpeciesList
      .flatMap(x => initializedSimulation.getAllPlant(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllPlant.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2))
      .foreach(world addEntity)



    val simulation = SimulationLoop(world, 250 millis)
    val aggregator = new DataAggregator(world entitiesState)
    simulation attachEraListener (era => {
      aggregator ingestData era
    })
    SimulationController(simulation, world entitiesState, aggregator ingestedData)
  }
}

object EntityBuilderHelpers {

  private val yearToClock = 10
  private val mutationProb = 0.05

  def distinctRandomPoints(n:Int, x:Int, y:Int):Set[Point] = {
    import scala.util.Random
    require(n < x * y)
    Stream.continually((Random.nextInt(x), Random.nextInt(y))).scanLeft(Set[Point]()) {
      (accumulator, el) => accumulator + Point(el._1, el._2)
    }.dropWhile(_.size < n).head
  }

  def initializeEntities(animals: Map[String, Int],
                         plants: Map[String, Int],
                         newAnimals: Map[CompleteAnimalData, Int],
                         newPlants: Map[CompletePlantData, Int],
                         worldHeight: Long ,
                         worldWidth: Long,
                         animalCreationFunction: (AnimalInfo, Point) => Entity): Seq[Entity] = {

    import scala.concurrent.ExecutionContext.Implicits.global
    val animalEntities: Seq[Entity] = animals.flatMap(entity => Seq.fill(entity._2)(entity._1))
      .zip(distinctRandomPoints(animals.values.sum, worldHeight.toInt, worldWidth.toInt))
      .map(entity => initializeEntity(GeneticsSimulator.newAnimal(entity._1), entity._2, worldHeight, worldWidth, animalCreationFunction)).toSeq

    val plantEntities: Seq[Entity] = plants.flatMap(entity => Seq.fill(entity._2)(entity._1))
        .zip(distinctRandomPoints(plants.values.sum, worldHeight.toInt, worldWidth.toInt))
        .map(entity => initializeEntity(GeneticsSimulator.newPlant(entity._1), entity._2)).toSeq

    val newAnimalEntities: Seq[Entity] = newAnimals.flatMap(entity => GeneticsSimulator.addNewAnimalSpecies(entity._1, entity._2))
      .zip(distinctRandomPoints(newAnimals.values.sum, worldHeight.toInt, worldWidth.toInt))
      .map(entity => initializeEntity(entity._1, entity._2, worldHeight, worldWidth, animalCreationFunction)).toSeq

    val newPlantEntities: Seq[Entity] = newPlants.flatMap(entity => GeneticsSimulator.addNewPlantSpecies(entity._1, entity._2))
      .zip(distinctRandomPoints(newPlants.values.sum, worldHeight.toInt, worldWidth.toInt))
      .map(entity => initializeEntity(entity._1, entity._2)).toSeq


    animalEntities ++ plantEntities ++ newAnimalEntities ++ newPlantEntities
  }

  def initializeEntity(animalInfo: AnimalInfo, position: Point, worldHeight: Long , worldWidth: Long, animalCreationFunction: (AnimalInfo, Point) => Entity)
                      (implicit executionContext: ExecutionContext): Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, animalInfo, position)
    entity addComponent initializeBrainComponent(entity, animalInfo, worldHeight, worldWidth)
    entity addComponent initializeAnimalPhysicalComponent(entity, animalInfo)
    entity addComponent initializeReproductionComponent(entity, animalInfo, animalCreationFunction)
    entity addComponent initializeOrgansTrackerComponent(entity)
    entity addComponent initializeInteractionTrackerComponent(entity)
    entity
  }

  def initializeEntity(plantInfo: PlantInfo, position: Point)
                      (implicit executionContext: ExecutionContext): Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, plantInfo, position)
    entity addComponent initializePlantPhysicalComponent(entity, plantInfo)
    entity addComponent initializePlantReproductionComponent(entity)
    entity
  }

  private def initializeBaseInfoComponent(entity: Entity, entityInfo: it.unibo.pps.ese.genetics.entities.EntityInfo, position: Point)
                                         (implicit executionContext: ExecutionContext): Component = {

    implicit def convertReign(reign: Reign): ReignType.Value = if (reign.reignName == "A") ReignType.ANIMAL else ReignType.PLANT
    implicit def convertDefense(entityInfo: it.unibo.pps.ese.genetics.entities.EntityInfo): Double =
      if (convertReign(entityInfo.species.reign) == ReignType.ANIMAL) entityInfo.qualities(ResistenceToAttack).qualityValue
      else entityInfo.qualities(Hardness).qualityValue

    BaseInfoComponent(
      entity specifications,
      entityInfo.species.name,
      convertReign(entityInfo.species.reign),
      entityInfo.gender.toString.toLowerCase,
      position,
      entityInfo.qualities(Height).qualityValue,
      entityInfo.qualities(NutritionalValue).qualityValue,
      convertDefense(entityInfo),
      entityInfo
    )
  }

  private def initializeBrainComponent(entity: Entity, animalInfo: AnimalInfo, worldHeight: Long , worldWidth: Long)
                                      (implicit executionContext: ExecutionContext): Component = {

    implicit def convertKind(dietType: DietType): String = if (dietType.dietName == "H") "herbivore" else "carnivorous"

    BrainComponent(entity specifications,
      worldHeight.toInt,
      worldWidth.toInt,
      animalInfo.qualities(Strength).qualityValue,
      animalInfo.qualities(RangeOfAction).qualityValue,
      animalInfo.qualities(FieldOfView).qualityValue,
      animalInfo.qualities(Attractiveness).qualityValue)
  }

  private def initializeAnimalPhysicalComponent(entity: Entity, animalInfo: AnimalInfo)
                                               (implicit executionContext: ExecutionContext): Component = {
    PhysicalStatusComponent(
      entity specifications,
      animalInfo.qualities(Life).qualityValue.toInt,
      animalInfo.qualities(EnergyRequirements).qualityValue,
      animalInfo.qualities(Maturity).qualityValue,
      animalInfo.qualities(Oldness).qualityValue,
      animalInfo.qualities(Decline).qualityValue,
      animalInfo.qualities(Speed).qualityValue,
      animalInfo.qualities(Fertility).qualityValue,
      yearToClock)
  }

  private def initializeReproductionComponent(entity: Entity, animalInfo: AnimalInfo, entitiesCreationFunction: (AnimalInfo, Point) => Entity)
                                             (implicit executionContext: ExecutionContext): Component = {
    ReproductionComponent(
      entity specifications,
      animalInfo.qualities.getOrElse(Fecundity, Quality(0, Fecundity)).qualityValue,
      GeneticsSimulator,
      animalInfo.genome,
      animalInfo.qualities.getOrElse(PregnancyDuration, Quality(0, PregnancyDuration)).qualityValue,
      yearToClock,
      mutationProb,
      animalInfo.qualities(EnergyRequirements).qualityValue,
      entitiesCreationFunction
    )
  }

  private def initializeOrgansTrackerComponent(entity: Entity)
                                              (implicit executionContext: ExecutionContext): Component = {
    new OrgansTrackerComponent(entity specifications)
  }

  private def initializeInteractionTrackerComponent(entity: Entity)
                                              (implicit executionContext: ExecutionContext): Component = {
    new InteractionTrackerComponent(entity specifications)
  }

  private def initializePlantPhysicalComponent(entity: Entity, plantInfo: PlantInfo)
                                              (implicit executionContext: ExecutionContext): Component = {
    PlantPhysicalComponent(
      entity specifications,
      yearToClock)
  }

  private def initializePlantReproductionComponent(entity: Entity)
                                              (implicit executionContext: ExecutionContext): Component = {
    PlantReproductionComponent(
      entity specifications,
      GeneticsSimulator,
      yearToClock)
  }
}
