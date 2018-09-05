package it.unibo.pps.ese.entitybehaviors

import java.util.UUID.randomUUID

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.{AnimalData, PlantData, SimulationData}
import it.unibo.pps.ese.genericworld.model.{EntityUpdateState, _}
import it.unibo.pps.ese.genericworld.model.support.BaseEvent
import it.unibo.pps.ese.genetics.dna.{AnimalGenome, MGene}
import it.unibo.pps.ese.genetics.dnaexpression.{AllelicBehaviour, GeneStats}
import it.unibo.pps.ese.genetics.{GeneticsSimulator, InitializedSimulation}
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, DietType, Gender, PlantInfo, Quality, QualityType, Species}
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
      new GeneticsSimulator {
        def beginSimulation(simulationData:SimulationData):InitializedSimulation = null
        def speciesList:Seq[String] = Seq()
        def plantSpeciesList:Seq[String] = Seq()
        def newPlant(species: String):PlantInfo = null
        def newAnimal(species:String):AnimalInfo = null
        def obtainMutantAlleles(species:String,gene:MGene):Seq[MGene] = Seq()
        def addNewAnimalSpecies(animalData:AnimalData,num:Int):Seq[AnimalInfo] = Seq()
        def addNewPlantSpecies(plantData:PlantData,num:Int):Seq[PlantInfo] = Seq()
        def getAnimalInfoByGenome(species:String,genome: AnimalGenome):AnimalInfo = new AnimalInfo {
          override def dietType: DietType = null

          override def genome: AnimalGenome = null

          override def qualities: Map[QualityType, Quality] = Map()

          override def activeAlleles: Seq[AllelicBehaviour] = Seq()

          override def species: Species = null

          override def gender: Gender = null
        }
        def getGeneStats(geneM:MGene, animalInfo: AnimalInfo):GeneStats = null
      },
      info.genome,
      3,
      -1,
      info.qualities(EnergyRequirements).qualityValue
    )
  }

  def baseEntityInit(animalInfo: AnimalInfo) : Entity = {
    val entity = Entity("improved", randomUUID().toString)
    entity addComponent initializeReproductionComponent(entity, animalInfo)
    entity
  }

  StaticRules.instance().addSpecies(Set("Test"))

  test("Copulation") {
    val world = World(10, 10)
    val data = YamlLoader.loadSimulation("it/unibo/pps/ese/entitybehaviors/util/reproduction/Simulation.yml")
    val initializedSimulation = GeneticsSimulator.beginSimulation(data)
    val male = behaviourEntityInit(baseEntityInit(initializedSimulation.getAllAnimals.head._2.head), Point(1,1), "male", None)
    val female = behaviourEntityInit(baseEntityInit(initializedSimulation.getAllAnimals.head._2.head), Point(2,2), "female", Some(male.specifications.id))
    world.addEntity(male)
    world.addEntity(female)
    Await.result(world.requireInfoUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    Await.result(world.requireStateUpdate, Duration.Inf)
    //Await.result(world.requireStateUpdate, Duration.Inf)
    //Await.result(world.requireStateUpdate, Duration.Inf)
  }

  def behaviourEntityInit(entity: Entity, position: Point, gender: String, active: Option[String]): Entity = {
    entity addComponent FakeComponent(entity.specifications, "Test", gender, position, active)
    entity
  }

  case class FakeStatusInfo(species: String, status: EntityUpdateState.Value) extends BaseEvent

  case class FakeComponent(override val entitySpecifications: EntitySpecifications,
                           species: String,
                           gender: String,
                           position: Point,
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
          println("send")
          publish(InteractionEntity(partner.get, ActionKind.COUPLE))
          partner = None
        }
        publish(new ComputeNextStateAck)
      case r: ReproductionBaseInformationRequest =>
        publish(ReproductionBaseInformationResponse(r id, gender, species))
      case r: ReproductionPhysicalInformationRequest =>
        publish(ReproductionPhysicalInformationResponse(r id, 400))
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

