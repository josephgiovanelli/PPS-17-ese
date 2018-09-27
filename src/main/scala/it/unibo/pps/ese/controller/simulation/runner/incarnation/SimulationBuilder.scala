package it.unibo.pps.ese.controller.simulation.runner.incarnation

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.simulation.DynamicRules
import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.simulation.loader.data.CompletePlantData
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.model.dataminer.DataAggregator
import it.unibo.pps.ese.controller.simulation.runner.core.UpdatableWorld.UpdatePolicy.Stochastic
import it.unibo.pps.ese.controller.simulation.runner.core.{Component, Entity, SimulationLoop, World}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators.SimulationController
import it.unibo.pps.ese.model.components.BaseInfoComponent
import it.unibo.pps.ese.model.components.animals.PhysicalStatusComponent
import it.unibo.pps.ese.model.components.animals.brain.BrainComponent
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.WorldRulesImpl
import it.unibo.pps.ese.model.components.animals.reproduction.ReproductionComponent
import it.unibo.pps.ese.model.components.animals.trackers.{InteractionTrackerComponent, OrgansTrackerComponent}
import it.unibo.pps.ese.model.components.plant.{PlantPhysicalComponent, PlantReproductionComponent}
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.entities.QualityType.{Attractiveness, _}
import it.unibo.pps.ese.model.genetics.entities.{AnimalInfo, DietType, PlantInfo, Quality, Reign}
import it.unibo.pps.ese.utils.Point
import it.unibo.pps.ese.controller.simulation.runner.core.support.RandomHelper._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.util.Random

object ReignType extends Enumeration {
  val ANIMAL, PLANT = Value
}

/**
  * Companion object of the simulation builder
  */
object SimulationBuilder {

  /**
    * Base Simulation trait
    */
  sealed trait Simulation
  object Simulation {

    /**
      * Simulation not ready for building
      */
    sealed trait EmptySimulation extends Simulation

    /**
      * Simulation with world's dimension. Not ready for building
      */
    sealed trait Dimension extends Simulation

    /**
      * Simulation with population data. Not ready for building
      */
    sealed trait Data extends Simulation

    /**
      * Simulation ready for building
      */
    type ReadySimulation = EmptySimulation with Dimension with Data
  }
}

/**
  * Playable simulation builder
  * @param width World width
  * @param height World height
  * @param data World population data
  * @param executionContext An execution context, required for async tasks
  * @tparam Simulation The state of the simulation
  */
class SimulationBuilder[Simulation <: SimulationBuilder.Simulation]
(width: Int = 0, height: Int = 0, data: CompleteSimulationData = null)(implicit executionContext: ExecutionContext) {

  import SimulationBuilder.Simulation._

  /**
    * Add world dimension info
    * @param width World width
    * @param height World height
    * @return The updated SimulationBuilder
    */
  def dimension(width: Int, height: Int): SimulationBuilder[Simulation with Dimension] =
    new SimulationBuilder(width, height, data)

  /**
    * Add world population data
    * @param data Population data
    * @return The updated SimulationBuilder
    */
  def data(data: CompleteSimulationData): SimulationBuilder[Simulation with Data] =
    new SimulationBuilder(width, height, data)

  /**
    * Build the simulation if ready
    * @param ev Check on the status of the simulation
    * @return A SimulationController instance, usable to control built Simulation lifecycle
    */
  def build(implicit ev: Simulation =:= ReadySimulation): SimulationController = controller

  private lazy val controller: SimulationController = {

    val attackThreshold = 10
    val heightThreshold = 7
    val couplingThreshold = 6
    val updatePeriod = 250 millis

    import EntityBuilderHelpers._

    val world = World[Stochastic](width, height)

    val geneticsSimulator = GeneticsSimulator
    val initializedSimulation = geneticsSimulator.beginSimulation(data)
    def animalCreationFunction: (AnimalInfo, Point) => Entity =
      (a, p) => EntityBuilderHelpers.initializeEntity(a, p, width, height, animalCreationFunction)

    DynamicRules.instance().addSpecies((geneticsSimulator.speciesList ++ geneticsSimulator.plantSpeciesList) toSet)
    val worldRules: WorldRulesImpl = WorldRulesImpl(attackThreshold,heightThreshold,couplingThreshold)
    DynamicRules.instance().setRules(worldRules)

    geneticsSimulator.speciesList
      .flatMap(x => initializedSimulation.getAllAnimals(x))
      .zip(Random.distinctRandomPoints(initializedSimulation.getAllAnimals.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2, height, width, animalCreationFunction))
      .foreach(world addEntity)

    geneticsSimulator.plantSpeciesList
      .flatMap(x => initializedSimulation.getAllPlant(x))
      .zip(Random.distinctRandomPoints(initializedSimulation.getAllPlant.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2))
      .foreach(world addEntity)


    Await.result(world.requireInfoUpdate, Duration.Inf)
    val simulation = SimulationLoop(world, updatePeriod)
    val aggregator = new DataAggregator(world entitiesState)
    simulation attachEraListener (era => {
      aggregator ingestData era
    })
    SimulationController(simulation, world entitiesState, aggregator ingestedData)
  }
}

/**
  * This object contains the utility methods used to build the simulation entities
  */
object EntityBuilderHelpers {

  private val yearToClock = 10
  private val mutationProb = 0.05

  /**
    * This method generates the entities to be added to the world.
    * @param animals existing species of animals and corresponding number of individuals to be added for each species
    * @param plants existing species of plants and corresponding number of individuals to be added for each species
    * @param newAnimals new species of animals and corresponding number of individuals to be added for each species
    * @param newPlants new species of plants and corresponding number of individuals to be added for each species
    * @param worldHeight the height of the world
    * @param worldWidth the width of the world
    * @param animalCreationFunction the function that generates through the dimensions of the world and the entity, each position of each entity
    * @param executionContext Execution context
    * @return the entities
    */
  def initializeEntities(animals: Map[String, Int],
                         plants: Map[String, Int],
                         newAnimals: Map[CompleteAnimalData, Int],
                         newPlants: Map[CompletePlantData, Int],
                         worldHeight: Long,
                         worldWidth: Long,
                         animalCreationFunction: (AnimalInfo, Point) => Entity)
                        (implicit executionContext: ExecutionContext): Seq[Entity] = {

    val animalEntities: Seq[Entity] = animals.flatMap(entity => Seq.fill(entity._2)(entity._1))
      .zip(Random.distinctRandomPoints(animals.values.sum, worldHeight.toInt, worldWidth.toInt))
      .map(entity => initializeEntity(GeneticsSimulator.newAnimal(entity._1), entity._2,
        worldHeight, worldWidth, animalCreationFunction)).toSeq

    val plantEntities: Seq[Entity] = plants.flatMap(entity => Seq.fill(entity._2)(entity._1))
        .zip(Random.distinctRandomPoints(plants.values.sum, worldHeight.toInt, worldWidth.toInt))
        .map(entity => initializeEntity(GeneticsSimulator.newPlant(entity._1), entity._2)).toSeq

    val newAnimalEntities: Seq[Entity] = newAnimals.flatMap(entity => GeneticsSimulator.addNewAnimalSpecies(entity._1, entity._2))
      .zip(Random.distinctRandomPoints(newAnimals.values.sum, worldHeight.toInt, worldWidth.toInt))
      .map(entity => initializeEntity(entity._1, entity._2, worldHeight, worldWidth, animalCreationFunction)).toSeq

    val newPlantEntities: Seq[Entity] = newPlants.flatMap(entity => GeneticsSimulator.addNewPlantSpecies(entity._1, entity._2))
      .zip(Random.distinctRandomPoints(newPlants.values.sum, worldHeight.toInt, worldWidth.toInt))
      .map(entity => initializeEntity(entity._1, entity._2)).toSeq


    animalEntities ++ plantEntities ++ newAnimalEntities ++ newPlantEntities
  }

  /**
    * Initialize an animal entity
    * @param animalInfo Info about the animal
    * @param position Entity start position
    * @param worldHeight World height
    * @param worldWidth World width
    * @param animalCreationFunction Strategy for the spawn process
    * @param executionContext Execution context
    * @return The built entity
    */
  def initializeEntity(animalInfo: AnimalInfo, position: Point, worldHeight: Long , worldWidth: Long, animalCreationFunction: (AnimalInfo, Point) => Entity)
                      (implicit executionContext: ExecutionContext): Entity = {
    val entity = Entity(randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, animalInfo, position)
    entity addComponent initializeBrainComponent(entity, animalInfo, worldHeight, worldWidth)
    entity addComponent initializeAnimalPhysicalComponent(entity, animalInfo)
    entity addComponent initializeReproductionComponent(entity, animalInfo, animalCreationFunction)
    entity addComponent initializeOrgansTrackerComponent(entity)
    entity addComponent initializeInteractionTrackerComponent(entity)
    entity
  }

  /**
    * Initialize a plant entity
    * @param plantInfo Info about the plant
    * @param position Entity start position
    * @param executionContext Execution context
    * @return The built entity
    */
  def initializeEntity(plantInfo: PlantInfo, position: Point)
                      (implicit executionContext: ExecutionContext): Entity = {
    val entity = Entity(randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, plantInfo, position)
    entity addComponent initializePlantPhysicalComponent(entity, plantInfo)
    entity addComponent initializePlantReproductionComponent(entity)
    entity
  }

  /**
    * Custom components initialization
    */

  private def initializeBaseInfoComponent(entity: Entity, entityInfo: it.unibo.pps.ese.model.genetics.entities.EntityInfo, position: Point)
                                         (implicit executionContext: ExecutionContext): Component = {

    implicit def convertReign(reign: Reign): ReignType.Value = if (reign.reignName == "A") ReignType.ANIMAL else ReignType.PLANT
    implicit def convertDefense(entityInfo: it.unibo.pps.ese.model.genetics.entities.EntityInfo): Double =
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
