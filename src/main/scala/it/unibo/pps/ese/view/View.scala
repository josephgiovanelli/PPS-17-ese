package it.unibo.pps.ese.view

import it.unibo.pps.ese.controller.loader.data.SimulationData
import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import it.unibo.pps.ese.genericworld.controller.{Controller, Observer}
import it.unibo.pps.ese.genericworld.model.{EntityInfo, SimulationBuilder}
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.configuration.{ConfigurationView, ConfigurationViewImpl}
import it.unibo.pps.ese.view.statistics.ChartsData
import scalafx.application.JFXApp.PrimaryStage

import scala.concurrent.{ExecutionContext, Future}

trait View extends PrimaryStage with WorldView with ConfigurationView {
  def addObserver(observer: Observer): Unit


  def eyes(active: Boolean)
  def brain(active: Boolean)
  def stomach(active: Boolean)
  def reproductionOrgan(active: Boolean)
  def pregnant(active: Boolean)
  def embryo(state: EmbryoStatus.Value)
}

trait MainComponent {
  def setScene(sceneType: ViewType.Value): Unit
  def getEntityDetails(id: String): Option[EntityInfo]
  def historicalData(): Future[ChartsData]
  def simulationEras(): Future[Seq[Long]]
  def entitiesInEra(era: Long): Future[Seq[String]]
  def setUp(simulationData: SimulationData)
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

  override def updateWorld(generation: Int, world: List[Entity]): Unit = {
    currentView match {
      case ViewType.MainView => mainView.updateWorld(generation, world)
      case _ =>
    }
  }

  override def getEntityDetails(id: String): Option[EntityInfo] = {
    observers.head.setWatched(id)
    observers.head.getEntityDetails(id)
  }

  override def setUp(simulationData: SimulationData): Unit =
    currentView match {
    case ViewType.ConfigurationView => {
      val controller: Controller = new SimulationBuilder[EmptySimulation].dimension(500, 500).data(simulationData).build
      controller.attachView(this, 30)
      controller.manage.play()
      setScene(ViewType.MainView)
    }
    case _ =>
  }

  override def eyes(active: Boolean): Unit = println("eyes:" + active)
  override def brain(active: Boolean): Unit = println("brain:" + active)
  override def stomach(active: Boolean): Unit = println("stomach:" + active)
  override def reproductionOrgan(active: Boolean): Unit = {
    println("reprduction organ:" + active)
    println("________________________________________")
  }
  override def embryo(state: EmbryoStatus.Value): Unit = println("embryo:" + state)
  override def pregnant(active: Boolean): Unit = println("pregnat:" + active)

  override def historicalData(): Future[ChartsData] = observers.head.historicalData()

  override def entitiesInEra(era: Long): Future[Seq[String]] = observers.head.entitiesInEra(era)

  override def simulationEras(): Future[Seq[Long]] = observers.head.simulationEras()
}

object ViewType extends Enumeration {
  type ViewType = Value
  val MainView, ConfigurationView = Value
}