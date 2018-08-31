package it.unibo.pps.ese.view

import it.unibo.pps.ese.genericworld.controller.{EntityDetails, Observer}
import scalafx.application.JFXApp.PrimaryStage

trait View extends PrimaryStage with WorldView with ConfigurationView {

  def addObserver(observer: Observer): Unit
}

trait MainComponent {
  def setScene(sceneType: ViewType.Value): Unit
  def getEntityDetails(id: String): EntityDetails
}

object View {
  def apply(): View = new ViewImpl()
}

private class ViewImpl extends View with MainComponent {

  var observers: List[Observer] = Nil
  var mainView: WorldView = new MainScene(this)
  var currentView: ViewType.Value = ViewType.MainView

  setScene(ViewType.MainView)

  override def addObserver(observer: Observer): Unit = {
    observers = observer :: observers
  }

  override def setScene(sceneType: ViewType.Value): Unit = {
    currentView = sceneType
    sceneType match {
      case ViewType.MainView =>
        val v = new MainScene(this)
        mainView = v
        this.scene = v
      case _ =>
    }
  }

  override def updateWorld(generation: Int, world: List[Entity]): Unit = {
    currentView match {
      case ViewType.MainView => mainView.updateWorld(generation, world)
      case _ =>
    }
  }

  override def getEntityDetails(id: String): EntityDetails = {
    observers.head.getEntityDetails(id)
  }
}

object ViewType extends Enumeration {
  type ViewType = Value
  val MainView, ConfigurationView = Value
}