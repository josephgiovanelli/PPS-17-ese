package it.unibo.pps.ese.entitywatchers

import it.unibo.pps.ese.dataminer.{DataMiner, Era, ReadOnlyEntityRepository, Species}
import it.unibo.pps.ese.entitybehaviors.StaticRules
import it.unibo.pps.ese.view.View

case class StoryTeller(consolidatedState: ReadOnlyEntityRepository) {

  var currentView: Option[View] = None

  def attachView(view: View): Unit =
    currentView = Some(view)

  def extinctSpecies(extinctSpecies: Seq[Species]): Unit =
    if (extinctSpecies.nonEmpty)
      extinctSpecies.foreach(x =>
        currentView.get extinctSpecies x)

  def mutantAlleles(genes: Seq[String]) =
    if (genes.nonEmpty) genes.foreach(x =>
      currentView.get mutantAllele x)

  def bornRegistry(era: Era) =
    StaticRules.instance().getSpecies().foreach(species => {
      val babies: Long = DataMiner(consolidatedState).bornCount(species, era)
      if (babies != 0) currentView.get bornRegistry (species, babies)
    })

  def deadRegistry(era: Era) =
    StaticRules.instance().getSpecies().foreach(species => {
      val dead: Long = DataMiner(consolidatedState).deadCount(species, era)
      if (dead != 0) currentView.get deadRegistry (species, dead)
    })

  def couplingRegistry(era: Era) =
    StaticRules.instance().getSpecies().foreach(species => {
      val entities: Long = DataMiner(consolidatedState).couplingCount(species, era)
      if (entities != 0) currentView.get bornRegistry (species, entities)
    })

}
