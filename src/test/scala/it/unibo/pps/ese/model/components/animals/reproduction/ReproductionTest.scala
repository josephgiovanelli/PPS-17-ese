package it.unibo.pps.ese.model.components.animals.reproduction

import it.unibo.pps.ese.controller.simulation.StaticRules
import it.unibo.pps.ese.controller.simulation.loader.YamlLoader
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.simulation.loader.io.File
import it.unibo.pps.ese.controller.simulation.runner.core.UpdatableWorld.UpdatePolicy.Deterministic
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.entities.{Female, Male}
import it.unibo.pps.ese.utils.Point
import org.kaikikm.threadresloader.ResourceLoader
import org.scalatest.WordSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import PartialEntitiesInitUtils._
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.WorldRulesImpl
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.WorldRulesImplUtils._

class ReproductionTest extends WordSpec {

  private val i = (9 to 1 by -1).iterator

  StaticRules.instance().addSpecies(Set("Gatto", "Giraffa", "ErbaGatta"))
  private val worldRules: WorldRulesImpl = WorldRulesImpl(Integer.MIN_VALUE, Integer.MAX_VALUE, 0,
    Set(("Gatto", "Giraffa"), ("Giraffa", "ErbaGatta")),
    Set(("Gatto", "Gatto"), ("Giraffa", "Giraffa")))
  StaticRules.instance().setRules(worldRules)

  private val data = YamlLoader.loadSimulation(File(ResourceLoader.getResource("it/unibo/pps/ese/controller/simulation/loader/Simulation.yml"))).asInstanceOf[CompleteSimulationData]
  private val initializedSimulation = GeneticsSimulator.beginSimulation(data)

  object IteratorsFactory {
    def lastCreatedMovesFirst: Iterator[Int] = (50 to 1 by -1).iterator
    def firstCreatedMovesFirst: Iterator[Int] = (1 to 50 by 1).iterator
  }

  private def checkPopulationIncrementAfterPeriod(erasNumber: Int, expectedPopulationIncrement: Int, world: World): Unit = {
    val startPopulation = world.entities.size
    (1 to erasNumber).foreach(_ => {
      assert(world.entities.size == startPopulation)
      Await.result(world.requireStateUpdate, Duration.Inf)
    })
    assert(world.entities.size == startPopulation + expectedPopulationIncrement)
  }

  private def checkPopulationStability(world: World): Unit = {
    val startPopulation = world.entities.size
    (1 to 20).foreach(_ => Await.result(world.requireStateUpdate, Duration.Inf))
    assert(world.entities.size == startPopulation)
  }
  private val maleInfo = initializedSimulation.getAllAnimals.head._2.filter(a => a.genome.sexualChromosomeCouple.gender == Male).head
  private val femaleInfo = initializedSimulation.getAllAnimals.head._2.filter(a => a.genome.sexualChromosomeCouple.gender == Female).head

  "Two fertile animals" must {
    "copulate" when {
      "female take action" when {
        def createMale(implicit i: Iterator[Int]): Entity = entityInit(maleInfo, Point(1,1), 1.0)
        def createFemale(implicit i: Iterator[Int], male: Entity): Entity = entityInit(femaleInfo, Point(2,2), 1.0, Some(male.specifications.id))
        "partner not already moved" in {
          val world = World[Deterministic](10, 10)
          implicit val i: Iterator[Int] = IteratorsFactory.lastCreatedMovesFirst
          implicit val male: Entity = createMale
          world.addEntity(male)
          world.addEntity(createFemale)
          Await.result(world.requireInfoUpdate, Duration.Inf)
          checkPopulationIncrementAfterPeriod(4, 3, world)
          checkPopulationStability(world)
        }
        "partner already moved" in {
          val world = World[Deterministic](10, 10)
          implicit val i: Iterator[Int] = IteratorsFactory.firstCreatedMovesFirst
          implicit val male: Entity = createMale
          world.addEntity(male)
          world.addEntity(createFemale)
          Await.result(world.requireInfoUpdate, Duration.Inf)
          checkPopulationIncrementAfterPeriod(5, 3, world)
          checkPopulationStability(world)
        }
      }
      "male take action" when {
        def createMale(implicit i: Iterator[Int], female: Entity): Entity = entityInit(maleInfo, Point(1,1), 1.0, Some(female.specifications.id))
        def createFemale(implicit i: Iterator[Int]): Entity = entityInit(femaleInfo, Point(2,2), 1.0)
        "partner not already moved" in {
          val world = World[Deterministic](10, 10)
          implicit val i: Iterator[Int] = IteratorsFactory.lastCreatedMovesFirst
          implicit val female: Entity = createFemale
          world.addEntity(createMale)
          world.addEntity(female)
          Await.result(world.requireInfoUpdate, Duration.Inf)
          checkPopulationIncrementAfterPeriod(4, 3, world)
          checkPopulationStability(world)
        }
        "partner already moved" in {
          implicit val i: Iterator[Int] = IteratorsFactory.firstCreatedMovesFirst
          implicit val female: Entity = createFemale
          val world = World[Deterministic](10, 10)
          world.addEntity(createMale)
          world.addEntity(female)
          Await.result(world.requireInfoUpdate, Duration.Inf)
          checkPopulationIncrementAfterPeriod(5, 3, world)
          checkPopulationStability(world)
        }
      }
    }
  }

  "Two animals with 0 fertility" must {
    "copulate producing 0 children" in {
      val world = World[Deterministic](10, 10)
      implicit val i: Iterator[Int] = IteratorsFactory.lastCreatedMovesFirst
      val male = entityInit(maleInfo, Point(1,1), 0.0)
      val female = entityInit(femaleInfo, Point(2,2), 0.0, Some(male.specifications.id))
      world.addEntity(male)
      world.addEntity(female)
      checkPopulationStability(world)
    }
  }

}

