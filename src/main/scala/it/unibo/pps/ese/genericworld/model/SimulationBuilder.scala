package it.unibo.pps.ese.genericworld.model

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.dataminer.DataAggregator
import it.unibo.pps.ese.entitybehaviors._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._
import it.unibo.pps.ese.genericworld.controller.{Controller, SimulationLoop}
import it.unibo.pps.ese.genericworld.model.UpdatableWorld.UpdatePolicy.{Deterministic, Stochastic}
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, DietType, PlantInfo, Quality, Reign}
import it.unibo.pps.ese.genetics.entities.QualityType.{Attractiveness, _}
import it.unibo.pps.ese.utils.Point

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

  def build(implicit ev: Simulation =:= ReadySimulation): Controller = controller

  private lazy val controller: Controller = {

    def distinctRandomPoints(n:Int, x:Int, y:Int):Set[Point] = {
      import scala.util.Random
      require(n < x * y)
      Stream.continually((Random.nextInt(x), Random.nextInt(y))).scanLeft(Set[Point]()) {
        (accumulator, el) => accumulator + Point(el._1, el._2)
      }.dropWhile(_.size < n).head
    }

    import EntityBuilderHelpers._

    StaticRules.instance().addSpecies(Set("Gatto", "Giraffa", "ErbaGatta"))
    val worldRules: WorldRulesImpl = decisionsupport.WorldRulesImpl.WorldRulesImpl(Integer.MIN_VALUE, (Integer.MIN_VALUE, Integer.MAX_VALUE), 0,
      Set(("Gatto", "Giraffa"), ("Giraffa", "ErbaGatta")),
      Set(("Gatto", "Gatto"), ("Giraffa", "Giraffa")))
    StaticRules.instance().setRules(worldRules)

    val world = World[Stochastic](width, height)

    val geneticsSimulator = GeneticsSimulator
    val initializedSimulation = geneticsSimulator.beginSimulation(data)

    geneticsSimulator.speciesList
      .flatMap(x => initializedSimulation.getAllAnimals(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllAnimals.map(z => z._2.size).sum, width, height))
      .map(x => initializeEntity(x._1, x._2, height, width))
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
    Controller(simulation, world entitiesState, aggregator ingestedData)
  }
}

object EntityBuilderHelpers {

  private val yearToClock = 10
  private val mutationProb = 0.05

  def initializeEntity(animalInfo: AnimalInfo, position: Point, worldHeight: Long , worldWidth: Long)
                      (implicit executionContext: ExecutionContext): Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, animalInfo, position)
    entity addComponent initializeBrainComponent(entity, animalInfo, worldHeight, worldWidth)
    entity addComponent initializeAnimalPhysicalComponent(entity, animalInfo)
    entity addComponent initializeReproductionComponent(entity, animalInfo)
    entity addComponent initializeOrgansTrackerComponent(entity)
    entity addComponent initializeInteractionTrackerComponent(entity)
    entity
  }

  def initializeEntity(plantInfo: PlantInfo, position: Point)
                      (implicit executionContext: ExecutionContext): Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeBaseInfoComponent(entity, plantInfo, position)
    entity addComponent initializePlantPhysicalComponent(entity, plantInfo)
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

  private def initializeReproductionComponent(entity: Entity, animalInfo: AnimalInfo)
                                             (implicit executionContext: ExecutionContext): Component = {
    ReproductionComponent(
      entity specifications,
      animalInfo.qualities.getOrElse(Fecundity, Quality(0, Fecundity)).qualityValue,
      GeneticsSimulator,
      animalInfo.genome,
      animalInfo.qualities.getOrElse(PregnancyDuration, Quality(0, PregnancyDuration)).qualityValue,
      yearToClock,
      mutationProb,
      animalInfo.qualities(EnergyRequirements).qualityValue
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
      plantInfo.qualities(Availability).qualityValue)
  }
}
