package it.unibo.pps.ese.view.core

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.simulation.loader.data.CompletePlantData
import it.unibo.pps.ese.controller.simulation.runner.core.data.{EntityInfo, EntityState}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators.{ChartsData, Observer, ReplayController, SimulationController}
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.sections.bodyviewer.AnimalInternalStatus
import it.unibo.pps.ese.view.sections.history.HistoryLog
import it.unibo.pps.ese.view.main.WorldView

import scala.concurrent.{ExecutionContext, Future}
import scalafx.Includes._
import scalafx.stage.{Stage, WindowEvent}

/**
  * Represents the main graphical component and contains all the methods visible from external non-view components
  */
trait View extends Stage with WorldView {
  /**
    * Adds an `Observer` to the current observers
    *
    * @param observer the observer to be added
    */
  def addObserver(observer: Observer): Unit

  /**
    * Updates the animal internal status
    *
    * @param animalInternalStatus
    */
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit

  /**
    * Updates the history log
    *
    * @param newLog
    */
  def updateHistoryLog(newLog:HistoryLog):Unit
}

/**
  * Allows the communication between the internal components of the view
  */
trait MainComponent {

  /**
    * Sets the current scene
    *
    * @param sceneType the `Scene` to be shown
    */
  def setScene(sceneType: ViewType.Value): Unit

  /**
    * Returns the entity details given the id
    *
    * @param id the entity id
    * @return the `entityDetails`
    */
  def getEntityDetails(id: String): Option[EntityInfo]

  /**
    * Sets the `Entity` to be watched
    *
    * @param id the id of the entity to be watched
    */
  def watchEntity(id:String):Unit

  /**
    * Stops to watch the given `Entity`
    *
    * @param id the id of the entity
    */
  def unwatchEntity(id:String):Unit

  /**
    * It allows to add entities at run time.
    * @param animals the animal species that are already in the simulation
    * @param plants the plant species that are already in the simulation
    * @param newAnimals the animal species that aren't in the simulation
    * @param newPlants the plant species that aren't in the simulation
    */
  def addEntities(animals: Map[String, Int], plants: Map[String, Int], newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit
  def historicalData(): Future[ChartsData]
  def simulationEras(): Future[Seq[Long]]
  def entitiesInEra(era: Long): Future[Seq[String]]
  def replay: ReplayController

  /**
    * Pauses the simulation
    */
  def pause()

  /**
    * Restarts the simulation
    */
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
  def apply(geneticsSimulator: GeneticsSimulator, controller: SimulationController)
           (implicit executionContext: ExecutionContext): View = new ViewImpl(geneticsSimulator, controller)
}

private class ViewImpl(geneticsSimulator: GeneticsSimulator, controller: SimulationController)
                      (implicit executionContext: ExecutionContext) extends View with MainComponent {

  var observers: List[Observer] = Nil
  var mainView: WorldView = new MainScene(geneticsSimulator,this)
  var currentView: ViewType.Value = ViewType.MainView
  setScene(ViewType.MainView)
  controller.manage.play()
  this.setOnCloseRequest((e: WindowEvent) => {
    controller.manage.exit()
  })
  this.show()

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

  override def pause(): Unit = observers.foreach(_.pause())

  override def play(): Unit = observers.foreach(_.play())
}

