package it.unibo.pps.ese.view

import scalafx.application.JFXApp.PrimaryStage

trait View extends PrimaryStage with WorldView with ConfigurationView {

  private var observers: List[Observer] = Nil

  def addObserver(observer: Observer): Unit = {
    observers = observer :: observers
  }

  def setScene(sceneType: ViewType.Value): Unit
}

object View {
  def apply(): View = new ViewImpl()
}

private class ViewImpl extends View {

  var mainView: WorldView = new MainScene()
  var currentView: ViewType.Value = ViewType.MainView

  setScene(ViewType.MainView)


  def setScene(sceneType: ViewType.Value): Unit = {
    currentView = sceneType
    sceneType match {
      case ViewType.MainView =>
        val v = new MainScene()
        mainView = v
        this.scene = v
      case _ =>
    }
  }

  override def updateWorld(world: List[Entity]): Unit = {
    currentView match {
      case ViewType.MainView => mainView.updateWorld(world)
      case _ =>
    }
  }
}

object ViewType extends Enumeration {
  type ViewType = Value
  val MainView, ConfigurationView = Value
}