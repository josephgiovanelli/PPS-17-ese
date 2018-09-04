package it.unibo.pps.ese.entitybehaviors

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.genericworld.model.{EntityUpdateState, _}
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
    val female = behaviourEntityInit(baseEntityInit(initializedSimulation.getAllAnimals.head._2.head, Point(1, 1), "F"), active = true)
    val male = behaviourEntityInit(baseEntityInit(initializedSimulation.getAllAnimals.head._2.head, Point(1, 1), "M"), active = false)
    world.addEntity(male)
    world.addEntity(female)
    Await.result(world.requireInfoUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
  }

  def behaviourEntityInit(entity: Entity, active: Boolean): Entity = {
    entity //addComponent FakeComponent(active)
  }

}

case class FakeStatusInfo(species: String, status: EntityUpdateState.Value) extends BaseEvent

case class FakeComponent(override val entitySpecifications: EntitySpecifications,
                             species: String,
                             gender: String,
                             var position: Point)
                        (implicit val executionContext: ExecutionContext)
                          extends WriterComponent(entitySpecifications) {

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case r: ReproductionBaseInformationRequest =>
        publish(ReproductionBaseInformationResponse(r id, gender, species))
    case ComputeNextState() =>
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      this synchronized {
        publish(FakeStatusInfo("", EntityUpdateState.WAITING))
      }
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[FakeStatusInfo]((classOf[FakeStatusInfo], ev => Seq(
      EntityProperty("species", ev species),
      EntityProperty("species", ev status)
    )))
  }
}

