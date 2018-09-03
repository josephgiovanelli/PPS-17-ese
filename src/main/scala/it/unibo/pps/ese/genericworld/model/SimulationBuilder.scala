package it.unibo.pps.ese.genericworld.model

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.SimulationData
import it.unibo.pps.ese.entitybehaviors.{BrainComponent, PhysicalStatusComponent, StaticRules, decisionsupport}
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._
import it.unibo.pps.ese.genericworld.controller.Controller
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, DietType, PlantInfo, Reign}
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
  (width: Int = 0, height: Int = 0, data: SimulationData = null)(implicit executionContext: ExecutionContext) {

  import SimulationBuilder.Simulation._

  def dimension(width: Int, height: Int): SimulationBuilder[Simulation with Dimension] =
    new SimulationBuilder(width, height, data)

  def data(data: SimulationData): SimulationBuilder[Simulation with Data] =
    new SimulationBuilder(width, height, data)

  def data(simulationConfigPath: String): SimulationBuilder[Simulation with Data] =
    new SimulationBuilder(width, height, YamlLoader.loadSimulation(simulationConfigPath))

  def build(implicit ev: Simulation =:= ReadySimulation): Controller = controller

  private lazy val controller: Controller = {

    def initializeAnimal(animalInfo: AnimalInfo, position: Point) : Entity = {
      val entity = Entity("improved", randomUUID().toString)
      entity addComponent initializeBaseInfoComponent(entity, animalInfo, position)
      entity addComponent initializeBrainComponent(entity, animalInfo)
      entity addComponent initializeAnimalPhysicalComponent(entity, animalInfo)
      entity
    }


    def initializePlant(plantInfo: PlantInfo, position: Point): Entity = {
      val entity = Entity("improved", randomUUID().toString)
      entity addComponent initializeBaseInfoComponent(entity, plantInfo, position)
      entity addComponent initializePlantPhysicalComponent(entity, plantInfo)
      entity
    }

    def initializeBrainComponent(entity: Entity, animalInfo: AnimalInfo): Component = {

      implicit def convertKind(dietType: DietType): String = if (dietType.dietName == "H") "herbivore" else "carnivorous"

      BrainComponent(entity specifications,
        height,
        width,
        animalInfo.qualities(Strength).qualityValue,
        animalInfo.qualities(RangeOfAction).qualityValue,
        animalInfo.qualities(FieldOfView).qualityValue,
        animalInfo.qualities(Attractiveness).qualityValue)
    }

    def initializeAnimalPhysicalComponent(entity: Entity, animalInfo: AnimalInfo): Component = {
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

    def initializeBaseInfoComponent(entity: Entity, entityInfo: it.unibo.pps.ese.genetics.entities.EntityInfo, position: Point): Component = {

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

    def initializePlantPhysicalComponent(entity: Entity, plantInfo: PlantInfo): Component = {
      PlantPhysicalComponent(
        entity specifications,
        plantInfo.qualities(Availability).qualityValue)
    }

    def distinctRandomPoints(n:Int, x:Int, y:Int):Set[Point] = {
      import scala.util.Random
      require(n < x * y)
      Stream.continually((Random.nextInt(x), Random.nextInt(y))).scanLeft(Set[Point]()) {
        (accumulator, el) => accumulator + Point(el._1, el._2)
      }.dropWhile(_.size < n).head
    }

    StaticRules.instance().addSpecies(Set("Gatto", "Giraffa", "ErbaGatta"))
    val worldRules: WorldRulesImpl = decisionsupport.WorldRulesImpl.WorldRulesImpl(Integer.MIN_VALUE, (0, Integer.MAX_VALUE), 0,
      Set(("Gatto", "Giraffa"), ("Giraffa", "ErbaGatta")),
      Set(("Gatto", "Gatto"), ("Giraffa", "Giraffa")))
    StaticRules.instance().setRules(worldRules)

    val world = World(width, height)

    val geneticsSimulator = GeneticsSimulator
    val initializedSimulation = geneticsSimulator.beginSimulation(data)

    geneticsSimulator.speciesList
      .flatMap(x => initializedSimulation.getAllAnimals(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllAnimals.map(z => z._2.size).sum, width, height))
      .map(x => initializeAnimal(x._1, x._2))
      .foreach(world addEntity)

    geneticsSimulator.plantSpeciesList
      .flatMap(x => initializedSimulation.getAllPlant(x))
      .zip(distinctRandomPoints(initializedSimulation.getAllPlant.map(z => z._2.size).sum, width, height))
      .map(x => initializePlant(x._1, x._2))
      .foreach(world addEntity)

    Await.result(world.requireInfoUpdate, Duration.Inf)
    Controller(world, 250 millis)
  }
}
