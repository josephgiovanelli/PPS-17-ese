package it.unibo.pps.ese.view

import it.unibo.pps.ese.controller.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.loader.data.CompletePlantData
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import it.unibo.pps.ese.genericworld.controller.{Controller, Observer}
import it.unibo.pps.ese.genericworld.model.{EntityInfo, SimulationBuilder}
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.configuration.{ConfigurationView, ConfigurationViewImpl}
import scalafx.application.JFXApp.PrimaryStage

trait View extends PrimaryStage with WorldView with ConfigurationView {
  def addObserver(observer: Observer): Unit
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit
  def updateHistoryLog(newLog:HistoryLog):Unit
}

trait MainComponent {
  def setScene(sceneType: ViewType.Value): Unit
  def getEntityDetails(id: String): Option[EntityInfo]
  def unwatchEntity(id:String):Unit
  def setUp(simulationData: CompleteSimulationData)
  def addEntities(animals: Map[String, Int], plants: Map[String, Int], newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit
}
trait BodyViewer {
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit
}
trait HistoryViewer{
  def updateHistoryLog(newLog:HistoryLog):Unit
}
object View {
  def apply(geneticsSimulator: GeneticsSimulator): View = new ViewImpl(geneticsSimulator)
}

private class ViewImpl(geneticsSimulator: GeneticsSimulator) extends View with MainComponent {

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

  override def setUp(simulationData: CompleteSimulationData): Unit =
    currentView match {
    case ViewType.ConfigurationView => {
      import scala.concurrent.ExecutionContext.Implicits.global
      val controller: Controller = new SimulationBuilder[EmptySimulation].dimension(500, 500).data(simulationData).build
      controller.attachView(this, 30)
      controller.manage.play()
      setScene(ViewType.MainView)
    }
    case _ =>
  }


  override def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus): Unit = {
    import Conversions._
    mainView.updateAnimalInternalStatus(animalInternalStatus)
  }


//  override def extinctSpecies(species: String): Unit = println("Specie " + species + " Estinta")
//  override def mutantAllele(gene: String): Unit = println("Alleli mutanti comparsi per il gene: " + gene)
//  override def bornRegistry(species: String, babies: Long): Unit = println("Sono nati " + babies + " entità della specie " + species)
//  override def deadRegistry(species: String, dead: Long): Unit = println("Sono morti " + dead + " entità della specie " + species)
//  override def couplingRegistry(species: String, entities: Long): Unit = println("Si sono accoppiati " + entities + " entità della specie " + species)
  override def updateHistoryLog(newLog: HistoryLog): Unit = {
//    newLog.extinctSpecies.foreach(s=>println("Specie " + s + " Estinta"))
//    newLog.mutantAlleles.foreach(gene=>println("Alleli mutanti comparsi per il gene: " + gene))
//    newLog.bornRegistry.foreach{case(species,babies)=>
//      println("Sono nati " + babies + " entità della specie " + species)
//    }
//    newLog.deadRegistry.foreach{case(species,dead)=>
//      println("Sono morti " + dead + " entità della specie " + species)
//    }
//    newLog.couplingRegistry.foreach{case(species,entities)=>
//      println("Si sono accoppiati " + entities + " entità della specie " + species)
//    }
  import Conversions._
  mainView.updateHistoryLog(newLog)
}

  override def unwatchEntity(id: String): Unit = {
    observers.foreach(_.unsetWatched(id))
  }

  override def addEntities(animals: Map[String, Int], plants: Map[String, Int], newAnimals: Map[CompleteAnimalData, Int], newPlants: Map[CompletePlantData, Int]): Unit = {
    observers.foreach(_.addEntities(animals, plants, newAnimals, newPlants))
  }

}

object ViewType extends Enumeration {
  type ViewType = Value
  val MainView, ConfigurationView = Value
}