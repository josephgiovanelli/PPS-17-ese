package it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers

import it.unibo.pps.ese.controller.simulation.StaticRules
import it.unibo.pps.ese.model.dataminer.DataMiner
import it.unibo.pps.ese.model.dataminer.DataModelSupport.{Era, Species}
import it.unibo.pps.ese.model.dataminer.datamodel.ReadOnlyEntityRepository
import it.unibo.pps.ese.view.sections.history.HistoryLog
import it.unibo.pps.ese.view.core.View

case class StoryTeller(miner: DataMiner) {

  var currentView: Option[View] = None

  def attachView(view: View): Unit =
    currentView = Some(view)

  def updateHistoryLog(era: Era) =
    if (currentView.isDefined) {
      val extinctSpecies: Seq[Species] = miner.extinctSpecies(era)
      val mutantAlleles: Seq[String] = miner.mutantAlleles(era)
      var bornRegistry: Map[String,Long] = Map.empty
      var deadRegistry: Map[String,Long] = Map.empty
      var couplingRegistry: Map[String,Long] = Map.empty
      StaticRules.instance().getSpecies().foreach(species => {
        val babies: Long = miner.bornCount(species, era)
        if (babies != 0) bornRegistry += (species -> babies)
      })
      StaticRules.instance().getSpecies().foreach(species => {
        val dead: Long = miner.deadCount(species, era)
        if (dead != 0) deadRegistry += (species -> dead)
      })
      StaticRules.instance().getSpecies().foreach(species => {
        val entities: Long = miner.couplingCount(species, era)
        if (entities != 0) couplingRegistry += (species -> entities)
      })
      currentView.get.updateHistoryLog(
        HistoryLog(extinctSpecies, mutantAlleles, bornRegistry, deadRegistry, couplingRegistry:Map[String,Long]))
    }

}
