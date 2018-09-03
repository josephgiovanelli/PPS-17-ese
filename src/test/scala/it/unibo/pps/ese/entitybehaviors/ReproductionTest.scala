package it.unibo.pps.ese.entitybehaviors

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.support.BaseEvent
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, Gender, Quality}
import it.unibo.pps.ese.genetics.entities.QualityType.{EnergyRequirements, Fecundity}
import it.unibo.pps.ese.utils.Point
import org.scalatest.FunSuite

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

class ReproductionTest extends FunSuite {

  def initializeReproductionComponent(entity: Entity, info: AnimalInfo): Component = {
    ReproductionComponent(
      entity specifications,
      info.qualities.getOrElse(Fecundity, Quality(0, Fecundity)).qualityValue,
      GeneticsSimulator,
      info.genome,
      3,
      0.1,
      info.qualities(EnergyRequirements).qualityValue
    )
  }

  def baseEntityInit(animalInfo: AnimalInfo, position: Point, gender: String) : Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeReproductionComponent(entity, animalInfo)
    entity
  }

  test("Chromosomes couples can be correctly mixed with mutations") {
    val world = World(10, 10)
    val data = YamlLoader.loadSimulation("it/unibo/pps/ese/entitybehaviors/util/reproduction/Simulation.yml")
    val initializedSimulation = GeneticsSimulator.beginSimulation(data)
    val male = baseEntityInit(initializedSimulation.getAllAnimals.head._2.head, Point(1, 1), "M")
    Await.result(world.requireInfoUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
  }
}

case class FakeStatusInfo(species: String) extends BaseEvent

case class FakeComponent(override val entitySpecifications: EntitySpecifications,
                             species: String,
                             reign: ReignType.Value,
                             gender: String,
                             var position: Point,
                             height: Double,
                             var nutritionalValue: Double,
                             defense: Double,
                             var elapsedClocks: Long = 0)
                        (implicit val executionContext: ExecutionContext)
                          extends WriterComponent(entitySpecifications) {

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case EntityNutritionalValue(newNutritionalValue) =>
      this synchronized {
        nutritionalValue = newNutritionalValue
      }
    case r: BaseInfoRequest =>
      this synchronized {
        publish(BaseInfoResponse(r id, species, reign, position, height, nutritionalValue, defense, gender, elapsedClocks))
      }
    case r: ReproductionBaseInformationRequest =>
      this synchronized {
        //TODO problem: elapsedClocks can be non-updated if ComputeNextState is served after reproduction
        /* Scenario:
         * scheduler->to all components async
         *            ->brain -> reproduction -> here
         *                                            -> BaseInfo
         */
        publish(ReproductionBaseInformationResponse(r id, gender, elapsedClocks, species))
      }
    case ComputeNextState() =>
      this synchronized {
        elapsedClocks += 1
      }
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      this synchronized {
        publish(BaseInfoResponse("", species, reign, position, height, nutritionalValue, defense, gender, elapsedClocks))
      }
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[FakeStatusInfo]((classOf[FakeStatusInfo], ev => Seq(
      EntityProperty("species", ev species)
    )))
  }
}

