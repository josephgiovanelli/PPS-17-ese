package it.unibo.pps.ese.view

import it.unibo.pps.ese.controller.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.loader.data.{CompletePlantData, SimulationData}
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.util.io.File
import it.unibo.pps.ese.genericworld.controller.{Controller, Observer, ReplayController}
import it.unibo.pps.ese.genericworld.model.{EntityInfo, EntityState, SimulationBuilder}
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.bodyViewer.AnimalInternalStatus
import it.unibo.pps.ese.view.configuration.{ConfigurationView, ConfigurationViewImpl}
import it.unibo.pps.ese.view.history.HistoryLog
import scalafx.application.JFXApp.PrimaryStage

import scala.concurrent.Future
import scala.util.Try
import it.unibo.pps.ese.controller.loader.data.SimulationData
import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import it.unibo.pps.ese.genericworld.controller.{Controller, Observer, ReplayController}
import it.unibo.pps.ese.genericworld.model.{EntityInfo, SimulationBuilder}
import it.unibo.pps.ese.view.statistics.ChartsData

import scala.concurrent.{ExecutionContext, Future}

trait View extends PrimaryStage with WorldView with ConfigurationView {
  def addObserver(observer: Observer): Unit
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit
  def updateHistoryLog(newLog:HistoryLog):Unit
}

trait MainComponent {
  def setScene(sceneType: ViewType.Value): Unit
  def getEntityDetails(id: String): Option[EntityInfo]
  def watchEntity(id:String):Unit
  def unwatchEntity(id:String):Unit
  def setUp(simulationData: CompleteSimulationData)
  def addEntities(animals: Map[String, Int], plants: Map[String, Int], newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit
  def historicalData(): Future[ChartsData]
  def simulationEras(): Future[Seq[Long]]
  def entitiesInEra(era: Long): Future[Seq[String]]
  def replay: ReplayController
  def pause()
  def play()
}
trait BodyViewer {
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit
  def clearStatus():Unit
}
trait HistoryViewer{
  def updateHistoryLog(newLog:HistoryLog):Unit
}
object View {
  def apply(geneticsSimulator: GeneticsSimulator)
           (implicit executionContext: ExecutionContext): View = new ViewImpl(geneticsSimulator)
}

private class ViewImpl(geneticsSimulator: GeneticsSimulator)
                      (implicit executionContext: ExecutionContext) extends View with MainComponent {

  var observers: List[Observer] = Nil
  var configurationView: ConfigurationView = null
  var mainView: WorldView = new MainScene(geneticsSimulator,this)
  var currentView: ViewType.Value = ViewType.MainView

  //da riaggiungere
  //setScene(ViewType.ConfigurationView)
  setScene(ViewType.MainView)

  override def addObserver(observer: Observer): Unit = {
    observers = observer :: observers
  }

  override def setScene(sceneType: ViewType.Value): Unit = {
    currentView = sceneType
    sceneType match {
      case ViewType.MainView =>
        val v = new MainScene(geneticsSimulator,this)
        mainView = v
        this.scene = v
      case ViewType.ConfigurationView => {
        val v = new ConfigurationViewImpl(this)
        configurationView = v
        this.scene = v
      }
    }
  }

  override def updateWorld(generation: Int, world: Seq[EntityState]): Unit = {
    currentView match {
      case ViewType.MainView => mainView.updateWorld(generation, world)
      case _ =>
    }
  }

  override def getEntityDetails(id: String): Option[EntityInfo] = {
    observers.head.getEntityDetails(id)
  }

  override def setUp(simulationData: CompleteSimulationData): Unit =
    currentView match {
    case ViewType.ConfigurationView => {
      val controller: Controller = new SimulationBuilder[EmptySimulation].dimension(500, 500).data(simulationData).build
      controller.attachView(this, 30)
      controller.manage.play()
      setScene(ViewType.MainView)
    }
    case _ =>
  }


  override def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus): Unit = {
    import it.unibo.pps.ese.view.utilities.Conversions._
    mainView.updateAnimalInternalStatus(animalInternalStatus)
  }

  override def updateHistoryLog(newLog: HistoryLog): Unit = {
    import it.unibo.pps.ese.view.utilities.Conversions._
    mainView.updateHistoryLog(newLog)
  }

  override def watchEntity(id: String): Unit = {
    println("Watching")
    observers.foreach(_.setWatched(id))
  }

  override def unwatchEntity(id: String): Unit = {
    println("Unwatching")
    observers.foreach(_.unsetWatched(id))
    import it.unibo.pps.ese.view.utilities.Conversions._
    mainView.clearStatus()
  }

  override def historicalData(): Future[ChartsData] = observers.head.historicalData()

  override def entitiesInEra(era: Long): Future[Seq[String]] = observers.head.entitiesInEra(era)

  override def simulationEras(): Future[Seq[Long]] = observers.head.simulationEras()

  override def replay: ReplayController = observers.head.replay

  override def addEntities(animals: Map[String, Int], plants: Map[String, Int], newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit = {
    observers.foreach(_.addEntities(animals, plants, newAnimals, newPlants))
  }

  override def pause(): Unit = observers.foreach(o => o.pause())

  override def play(): Unit = observers.foreach(o => o.play())
}

object ViewType extends Enumeration {
  type ViewType = Value
  val MainView, ConfigurationView = Value
}