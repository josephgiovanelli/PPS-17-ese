package it.unibo.pps.ese.genericworld.controller

import java.io.File
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import it.unibo.pps.ese.controller.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.loader.data.CompletePlantData
import it.unibo.pps.ese.dataminer.{DataMiner, DataSaver, ReadOnlyEntityRepository}
import it.unibo.pps.ese.entitywatchers.{StoryTeller, Surgeon}
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.view.View
import it.unibo.pps.ese.view.statistics.ChartsData

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait SimulationController {
  //Partenza simulazione
  //def startSimulation(file: File): Try[Unit]
  //Editing simulazione
  //def loadSimulation(file: File): Try[PartialSimulationData]
  //Cachare saver e target
  //def saveSimulationData(simulation: PartialSimulationData, target: File): Try[Unit]
  //def retrySave(overrideResource: Option[ExistingResource])

  def attachView(view: View, frameRate: Int): Unit
  def manage: ManageableController
  def query: QueryableController
}
trait ManageableController {
  def play(): Unit
  def pause(): Unit
  def exit(): Unit
  def add(animals: Map[String, Int], plants: Map[String, Int],
          newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit
  def isPlaying: Boolean
  def isStopped: Boolean
}
trait QueryableController {
  def entityData(id: String): Option[EntityState]
  def watch(entity: String): Unit
  def unwatch(): Unit
  def historicalData(): Future[ChartsData]
  def simulationEras(): Future[Seq[Long]]
  def entitiesInEra(era: Long): Future[Seq[String]]
  def replay: ReplayController
}

trait BaseManageableController extends ManageableController {

  private[this] var _stop = false
  private[this] var _paused = true

  def simulation: SimulationLoop

  def add(animals: Map[String, Int], plants: Map[String, Int],
                   newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit = {
    simulation.addEntities(animals, plants, newAnimals, newPlants)
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

trait BaseQueryableController extends QueryableController {

  implicit val executionContext: ExecutionContext

  def realTimeState: ReadOnlyEntityState
  def consolidatedState: ReadOnlyEntityRepository


  def entityData(id: String): Option[EntityState] =
    realTimeState getFilteredState(x => x.entityId == id) match {
      case Seq(single) => Some(single)
      case _ => None
    }

  def historicalData(): Future[ChartsData] = {
    Future {
      val miner = DataMiner(consolidatedState)
      val populationTrend = miner populationTrend()
      val startEra = miner startEra
      val lastEra = miner lastEra
      val populationDistribution = (miner aliveSpecies lastEra) map (x => (x, miner aliveCount(x, lastEra)))
      val births = (miner aliveSpecies lastEra) map (x =>
        (x, (startEra to lastEra) map (y => (y, miner bornCount(x, y)))))
      val mutations = (miner aliveSpecies lastEra) map (x =>
        (x toString, (startEra to lastEra) map (y => (y, (miner mutantAlleles(x, y) size) toLong))))
      ChartsData(
        Seq[(String, Seq[(Long, Long)])](("global", populationTrend.map(x => (x._1, x._2)))),
        populationDistribution,
        births,
        mutations)
    }
  }

  def simulationEras(): Future[Seq[Long]] =
    Future {(DataMiner(consolidatedState) startEra) to (DataMiner(consolidatedState) lastEra)}

  def entitiesInEra(era: Long): Future[Seq[String]] =
    Future {consolidatedState entitiesInEra era filter (x => x.structuralData.reign == ReignType.ANIMAL.toString) map(x => x.id)}

  def replay: ReplayController = ReplayController(consolidatedState)
}

trait SingleViewController extends SimulationController with BaseManageableController with BaseQueryableController {

  private[this] val surgeon = Surgeon(realTimeState)
  private[this] val storyTeller = StoryTeller(consolidatedState)
  private[this] val _era: AtomicInteger = new AtomicInteger(1)

  simulation attachEraListener(era => _era set era.toInt)
  consolidatedState attachNewDataListener(era => storyTeller updateHistoryLog era)

  override def watch(entity: String): Unit = surgeon inspects entity

  override def unwatch(): Unit = surgeon leaves()

  def attachView(view: View, frameRate: Int): Unit = {
    storyTeller attachView view
    import ViewHelpers.{ManageableObserver, toViewData}
    view addObserver this
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

  def apply(simulation: SimulationLoop, realTimeState: ReadOnlyEntityState, consolidatedState: ReadOnlyEntityRepository)
           (implicit executionContext: ExecutionContext): SimulationController =
    BaseController(simulation, realTimeState, consolidatedState)

  private case class BaseController(simulation: SimulationLoop,
                                    realTimeState: ReadOnlyEntityState,
                                    consolidatedState: ReadOnlyEntityRepository)
                                   (implicit val executionContext: ExecutionContext) extends SingleViewController
}
