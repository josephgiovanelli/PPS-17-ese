package it.unibo.pps.ese.view

import scalafx.scene.control.{ListView, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import it.unibo.pps.ese.view.speciesdetails.TextUtilities._
import javafx.collections.{FXCollections, ObservableList}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Orientation

trait HistoryPane extends ScrollPane{
  def updateHistoryLog(newLog:HistoryLog):Unit
}
object HistoryPane{
  def apply(): HistoryPane = new HistoryPaneImpl()
  private class HistoryPaneImpl() extends HistoryPane {
    val mainPane = new BorderPane()
    val vbox = new VBox()
    vbox.translateY = 10
    mainPane.center = vbox
    content = mainPane
    val logsList:ObservableList[String] = FXCollections.observableArrayList()
    val logs:ListView[String] = new ListView[String](logsList)
    logs.prefWidth <== width
    logs.prefHeight <== height
    logs.orientation = Orientation.Vertical
    vbox.spacing = 10
    val title:HBox = "The World History".toHBox
    title.prefWidth <== width
    vbox.children = List(title,logs)
    override def updateHistoryLog(newLog: HistoryLog): Unit = {
      println(newLog)
      Platform.runLater{
        ()->{
          newLog.extinctSpecies.foreach(s=>logsList.add("Specie " + s + " Estinta"))
          newLog.mutantAlleles.foreach(gene=>logsList.add("Alleli mutanti comparsi per il gene: " + gene))
          newLog.bornRegistry.foreach{case(species,babies)=>
            logsList.add("Sono nati " + babies + " entità della specie " + species)
          }
          newLog.deadRegistry.foreach{case(species,dead)=>
            logsList.add("Sono morti " + dead + " entità della specie " + species)
          }
          newLog.couplingRegistry.foreach{case(species,entities)=>
            logsList.add("Si sono accoppiati " + entities + " entità della specie " + species)
          }
        }
      }
    }
  }
}
