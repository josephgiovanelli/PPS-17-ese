package it.unibo.pps.ese.model.components.animals.reproduction

import it.unibo.pps.ese.controller.simulation.StaticRules
import it.unibo.pps.ese.controller.simulation.loader.YamlLoader
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.simulation.loader.io.File
import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.BaseEvent
import it.unibo.pps.ese.controller.simulation.runner.core.UpdatableWorld.UpdatePolicy.Deterministic
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.controller.simulation.runner.core.{EntityUpdateState, _}
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.WorldRulesImpl
import it.unibo.pps.ese.model.components.animals.brain.{ActionTypes, Couple, InteractionEntity}
import it.unibo.pps.ese.model.components.{BaseInfoRequest, BaseInfoResponse}
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.entities.QualityType.{EnergyRequirements, Fecundity}
import it.unibo.pps.ese.model.genetics.entities.{AnimalInfo, Female, Male, Quality}
import it.unibo.pps.ese.utils.Point
import org.kaikikm.threadresloader.ResourceLoader
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

class ReproductionTest extends FunSuite {

  private val i = (9 to 1 by -1).iterator

  import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.WorldRulesImplUtils._
  StaticRules.instance().addSpecies(Set("Gatto", "Giraffa", "ErbaGatta"))
  private val worldRules: WorldRulesImpl = WorldRulesImpl(Integer.MIN_VALUE, Integer.MAX_VALUE, 0,
    Set(("Gatto", "Giraffa"), ("Giraffa", "ErbaGatta")),
    Set(("Gatto", "Gatto"), ("Giraffa", "Giraffa")))
  StaticRules.instance().setRules(worldRules)

  private val data = YamlLoader.loadSimulation(File(ResourceLoader.getResource("it/unibo/pps/ese/controller/simulation/loader/Simulation.yml"))).asInstanceOf[CompleteSimulationData]
  private val geneticsSimulator = GeneticsSimulator
  private val initializedSimulation = geneticsSimulator.beginSimulation(data)

  test("Copulation") {
    val world = World[Deterministic](10, 10)
    val maleInfo = initializedSimulation.getAllAnimals.head._2.filter(a => a.genome.sexualChromosomeCouple.gender == Male).head
    val femaleInfo = initializedSimulation.getAllAnimals.head._2.filter(a => a.genome.sexualChromosomeCouple.gender == Female).head
    val male = entityInit(maleInfo, Point(1,1), 1.0)
    val female = entityInit(femaleInfo, Point(2,2), 1.0, Some(male.specifications.id))
    world.addEntity(male)
    world.addEntity(female)
    Await.result(world.requireInfoUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    assert(world.entities.size == 5)
  }

  //TODO one sufficient
  test("Two animals with 0 fertility can copulate, but without producing children") {
    val world = World[Deterministic](10, 10)
    val maleInfo = initializedSimulation.getAllAnimals.head._2.filter(a => a.genome.sexualChromosomeCouple.gender == Male).head
    val femaleInfo = initializedSimulation.getAllAnimals.head._2.filter(a => a.genome.sexualChromosomeCouple.gender == Female).head
    val male = entityInit(maleInfo, Point(1,1), 0.0)
    val female = entityInit(femaleInfo, Point(2,2), 0.0, Some(male.specifications.id))
    world.addEntity(male)
    world.addEntity(female)
    Await.result(world.requireInfoUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    assert(world.entities.size == 2)
    Await.result(world.requireStateUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
  }

  def entityInit(animalInfo: AnimalInfo, position: Point, fertility: Double, active: Option[String] = None): Entity = {
    var gender = ""
    if(animalInfo.gender == Male) {
      gender = "male"
    } else if(animalInfo.gender == Female) {
      gender = "female"
    }
    behaviourEntityInit(
      baseEntityInit(animalInfo),
      animalInfo,
      position,
      gender,
      fertility,
      active)
  }

  def baseEntityInit(animalInfo: AnimalInfo) : Entity = {
    val entity = Entity(i.next().toString)
    entity addComponent initializeReproductionComponent(entity, animalInfo)
    entity
  }

  def initializeReproductionComponent(entity: Entity, info: AnimalInfo): Component = {
    def animalCreationFunction: (AnimalInfo, Point) => Entity = (a, p) => entityInit(a, p, 0.0)
    ReproductionComponent(
      entity specifications,
      info.qualities.getOrElse(Fecundity, Quality(0, Fecundity)).qualityValue,
      GeneticsSimulator,
      info.genome,
      0.5,
      6,
      -1,
      info.qualities(EnergyRequirements).qualityValue,
      animalCreationFunction
    )
  }

  def behaviourEntityInit(entity: Entity, info: AnimalInfo, position: Point, gender: String, fertility: Double, active: Option[String]): Entity = {
    entity addComponent FakeComponent(entity.specifications, info.species.name, gender, position, fertility, active)
    entity
  }



  case class FakeStatusInfo(species: String, status: EntityUpdateState.Value) extends BaseEvent

  case class FakeComponent(override val entitySpecifications: EntitySpecifications,
                           species: String,
                           gender: String,
                           position: Point,
                           fertility: Double,
                           var partner: Option[String])
                          (implicit val executionContext: ExecutionContext)
    extends WriterComponent(entitySpecifications) {

    override def initialize(): Unit = {
      subscribeEvents()
      configureMappings()
    }

    private def subscribeEvents(): Unit = subscribe {
      case ComputeNextState() =>
        if(partner.nonEmpty) {
          publish(InteractionEntity(partner.get, Couple))
          partner = None
        }
        publish(new ComputeNextStateAck)
      case r: BaseInfoRequest =>
        publish(BaseInfoResponse(r.id, species, null, position, 0, 0, 0, "", null))
      case r: ReproductionBaseInformationRequest =>
        publish(ReproductionBaseInformationResponse(r id, gender, species))
      case r: ReproductionPhysicalInformationRequest =>
        publish(ReproductionPhysicalInformationResponse(r id, fertility))
      case GetInfo() =>
        this synchronized {
          publish(FakeStatusInfo(species, EntityUpdateState.WAITING))
        }
        publish(new GetInfoAck)
      case _ => Unit
    }

    private def configureMappings(): Unit = {
      addMapping[FakeStatusInfo]((classOf[FakeStatusInfo], ev => Seq(
        EntityProperty("species", ev species),
        EntityProperty("status", ev status)
      )))
    }
  }

}

