package it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers

import it.unibo.pps.ese.controller.simulation.StaticRules
import it.unibo.pps.ese.model.dataminer.{DataMiner, Era, ReadOnlyEntityRepository, Species}
import it.unibo.pps.ese.view.sections.history.HistoryLog
import it.unibo.pps.ese.view.core.View

case class StoryTeller(consolidatedState: ReadOnlyEntityRepository) {

  var currentView: Option[View] = None

  def attachView(view: View): Unit =
    currentView = Some(view)

  def updateHistoryLog(era: Era) =
    if (currentView.isDefined) {
      val extinctSpecies: Seq[Species] = DataMiner(consolidatedState).extinctSpecies(era)
      val mutantAlleles: Seq[String] = DataMiner(consolidatedState).mutantAlleles(era)
      var bornRegistry: Map[String,Long] = Map.empty
      var deadRegistry: Map[String,Long] = Map.empty
      var couplingRegistry: Map[String,Long] = Map.empty
      StaticRules.instance().getSpecies().foreach(species => {
        val babies: Long = DataMiner(consolidatedState).bornCount(species, era)
        if (babies != 0) bornRegistry += (species -> babies)
      })
      StaticRules.instance().getSpecies().foreach(species => {
        val dead: Long = DataMiner(consolidatedState).deadCount(species, era)
        if (dead != 0) deadRegistry += (species -> dead)
      })
      StaticRules.instance().getSpecies().foreach(species => {
        val entities: Long = DataMiner(consolidatedState).couplingCount(species, era)
        if (entities != 0) couplingRegistry += (species -> entities)
      })
      currentView.get.updateHistoryLog(
        HistoryLog(extinctSpecies, mutantAlleles, bornRegistry, deadRegistry, couplingRegistry:Map[String,Long]))
    }

}
