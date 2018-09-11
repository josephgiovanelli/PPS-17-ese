package it.unibo.pps.ese.view

import scalafx.scene.control.ScrollPane
import scalafx.scene.layout.{BorderPane, VBox}
import it.unibo.pps.ese.view.speciesdetails.TextUtilities._
import scalafx.Includes._

trait HistoryPane extends ScrollPane{
  def updateHistoryLog(newLog:HistoryLog):Unit
}
object HistoryPane{
  def apply(): HistoryPane = new HistoryPaneImpl()
  private class HistoryPaneImpl() extends HistoryPane {
    val mainPane = new BorderPane()
    val vbox = new VBox()
    mainPane.center = vbox
    content = mainPane
    vbox.children = List("Ciao".toHBox)
    override def updateHistoryLog(newLog: HistoryLog): Unit = {
      println(newLog)
      println(vbox.getChildren.size())

      newLog.extinctSpecies.foreach(s=>vbox.children+= ("Specie " + s + " Estinta").toHBox)
      newLog.mutantAlleles.foreach(gene=>vbox.children+=("Alleli mutanti comparsi per il gene: " + gene).toHBox)
      newLog.bornRegistry.foreach{case(species,babies)=>
        println("Ciaaoo")
        vbox.children+=("Sono nati " + babies + " entità della specie " + species).toHBox
      }
      newLog.deadRegistry.foreach{case(species,dead)=>
        vbox.children+=("Sono morti " + dead + " entità della specie " + species).toHBox
      }
      newLog.couplingRegistry.foreach{case(species,entities)=>
        vbox.children+=("Si sono accoppiati " + entities + " entità della specie " + species).toHBox
      }
    }
  }
}
