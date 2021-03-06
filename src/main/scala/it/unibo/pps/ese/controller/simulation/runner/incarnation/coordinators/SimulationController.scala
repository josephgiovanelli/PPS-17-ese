package it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators

import java.util.concurrent.atomic.AtomicInteger

import it.unibo.pps.ese.controller.simulation.DynamicRules
import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.simulation.loader.data.CompletePlantData
import it.unibo.pps.ese.model.dataminer.DataMiner
import it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers.{StoryTeller, Surgeon}
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.data.{EntityState, ReadOnlyEntityState}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.{EntityBuilderHelpers, ReignType}
import it.unibo.pps.ese.model.dataminer.datamodel.ReadOnlyEntityRepository
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo
import it.unibo.pps.ese.utils.Point
import it.unibo.pps.ese.view.core.View

import scala.concurrent.{ExecutionContext, Future}

/**
  * This Trait contains the main APIs necessary to manage and query a running Simulation
  */
trait SimulationController {
  /**
    * Register a view module to be notified with simulation data for visualization purposes
    * @param view The View to be registered
    * @param frameRate Simulation data sample period. This value determines how fast the controller
    *                  will notify the attached View with the Simulation data
    */
  def attachView(view: View, frameRate: Int): Unit

  /**
    * Get a manageable interface for the running simulation.
    * @return A ManageableController instance
    */
  def manage: ManageableController

  /**
    * Get a queryable interface for the running simulation.
    * @return A ManageableController instance
    */
  def query: QueryableController
}

/**
  * This Trait contains APIs related to simulation lifecycle management
  */
trait ManageableController {

  /**
    * Run the target simulation
    */
  def play(): Unit

  /**
    * Pause the target simulation
    */
  def pause(): Unit

  /**
    * Dispose the target simulation
    */
  def exit(): Unit

  /**
    * This method generates the entities to be added to the world.
    * @param animals existing species of animals and corresponding number of individuals to be added for each species
    * @param plants existing species of plants and corresponding number of individuals to be added for each species
    * @param newAnimals new species of animals and corresponding number of individuals to be added for each species
    * @param newPlants new species of plants and corresponding number of individuals to be added for each species
    */
  def add(animals: Map[String, Int], plants: Map[String, Int],
          newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit

  /**
    * Check if the target simulation is running
    * @return True if the simulation is running, false otherwise
    */
  def isPlaying: Boolean

  /**
    * Check if the target simulation has been disposed
    * @return True if the simulation has been disposed, false otherwise
    */
  def isStopped: Boolean
}

/**
  * This Trait contains APIs related to simulation data interrogation
  */
trait QueryableController {

  /**
    * Get info about selected entity
    * @param id The entity identifier
    * @return Entity's info if available
    */
  def entityData(id: String): Option[EntityState]

  /**
    * It tells to the surgeon the entity to inspect.
    * @param entity the identifier of the entity to inspect
    */
  def watch(entity: String): Unit

  /**
    * It tells to the surgeon that can leaves the entity.
    */
  def unwatch(): Unit

  /**
    * Get statistical data about the running simulation
    * @return A future that will be completed with the requested data
    */
  def historicalData(): Future[ChartsData]

  /**
    * Get a list of the eras already calculated by the running simulation
    * @return A Future that will be completed with the requested data
    */
  def simulationEras(): Future[Seq[Long]]

  /**
    * Get a list of the entities who were alive in the selected era
    * @return A Future that will be completed with the requested data
    */
  def entitiesInEra(era: Long): Future[Seq[String]]

  /**
    * Get a new instance of the ReplayController trait, necessary to replay an entity's life
    * @return A ReplayController instance
    */
  def replay: ReplayController
}

/**
  * ManageableController trait implementation
  */
trait BaseManageableController extends ManageableController {

  implicit val executionContext: ExecutionContext

  private[this] var _stop = false
  private[this] var _paused = true

  def simulation: SimulationLoop

  def add(animals: Map[String, Int], plants: Map[String, Int],
                   newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit = {
    val worldInfo = simulation worldInfo()
    def animalCreationFunction: (AnimalInfo, Point) => Entity =
      (a, p) => EntityBuilderHelpers.initializeEntity(a, p, worldInfo.width, worldInfo.height, animalCreationFunction)
    val entities: Seq[Entity] = EntityBuilderHelpers.initializeEntities(animals, plants, newAnimals, newPlants,
      worldInfo.width, worldInfo.height, animalCreationFunction)
    DynamicRules.instance().updateRules()
    simulation addEntities entities
  }

  def play(): Unit = this synchronized {
    simulation play()
    _paused = false
    notify()
  }

  def pause(): Unit = this synchronized {
    simulation pause()
    _paused = true
  }

  def exit(): Unit = this synchronized {
    simulation dispose()
    _stop = true
    _paused = true
    notify()
  }

  def isPlaying: Boolean = !_paused
  def isStopped: Boolean = _stop
}

/**
  * QueryableController trait implementation
  */
trait BaseQueryableController extends QueryableController {

  implicit val executionContext: ExecutionContext

  def realTimeState: ReadOnlyEntityState
  def consolidatedState: ReadOnlyEntityRepository
  val miner: DataMiner = DataMiner(consolidatedState)

  def entityData(id: String): Option[EntityState] =
    realTimeState getFilteredState(x => x.entityId == id) match {
      case Seq(single) => Some(single)
      case _ => None
    }

  def historicalData(): Future[ChartsData] = {
    Future {
      val startEra = miner.startEra
      val lastEra = miner.lastEra
      ChartsData(
        Seq[(String, Seq[(Long, Long)])](("Global", miner populationTrend() map(x => (x._1, x._2)))),
        (miner aliveSpecies lastEra) map (x => (x, miner aliveCount(x, lastEra))),
        (miner aliveSpecies lastEra) map (x => (x, (startEra to lastEra) map (y => (y, miner bornCount(x, y))))),
        (miner aliveSpecies lastEra) map (x => (x toString, (startEra to lastEra) map (y =>
          (y, (miner mutantAlleles(x, y) size) toLong))))
      )
    }
  }

  def simulationEras(): Future[Seq[Long]] =
    Future {(miner startEra) to (miner lastEra)}

  def entitiesInEra(era: Long): Future[Seq[String]] =
    Future {consolidatedState entitiesInEra era filter (x => x.structuralData.reign ==
      ReignType.ANIMAL.toString) map(x => x.id)}

  def replay: ReplayController = ReplayController(consolidatedState)
}

/**
  * SimulationController trait implementation
  */
trait SingleViewController extends SimulationController with BaseManageableController with BaseQueryableController {

  private[this] val surgeon = Surgeon(realTimeState)
  private[this] val storyTeller = StoryTeller(miner)
  private[this] val _era: AtomicInteger = new AtomicInteger(0)
  private[this] var _attached = false

  simulation attachEraListener(era => _era set era.toInt)
  consolidatedState attachNewDataListener(era => storyTeller updateHistoryLog era)

  override def watch(entity: String): Unit = surgeon inspects entity

  override def unwatch(): Unit = surgeon leaves()

  def attachView(view: View, frameRate: Int): Unit = if (!_attached){
    _attached = true
    storyTeller attachView view
    import ViewHelpers.ManageableObserver
    view addObserver this
    play()
    new Thread (() => {
      while(!isStopped) {
        normalizeFrameRate(() => {
          if (!isPlaying) this synchronized wait()
          view updateWorld (_era get(), realTimeState getFilteredState(_ => true))
          surgeon informAboutOrgansStatus view
        }, frameRate)
      }
    }) start()
  }

  override def manage: ManageableController = this

  override def query: QueryableController = this

  private def normalizeFrameRate(job: () => Unit, fps: Int): Unit = {
    val start = System.currentTimeMillis()
    job()
    val stop = System.currentTimeMillis()
    if (stop - start < 1000 / fps) {
      Thread.sleep((1000 / fps) - (stop - start))
    }
  }
}

object SimulationController {

  /**
    * @param simulation The Simulation to be managed
    * @param realTimeState The real time data source
    * @param consolidatedState The historical data source
    * @param executionContext An execution context, required for async tasks
    * @return A SimulationController instance
    */
  def apply(simulation: SimulationLoop, realTimeState: ReadOnlyEntityState, consolidatedState: ReadOnlyEntityRepository)
           (implicit executionContext: ExecutionContext): SimulationController =
    BaseController(simulation, realTimeState, consolidatedState)

  private case class BaseController(simulation: SimulationLoop,
                                    realTimeState: ReadOnlyEntityState,
                                    consolidatedState: ReadOnlyEntityRepository)
                                   (implicit val executionContext: ExecutionContext) extends SingleViewController
}
