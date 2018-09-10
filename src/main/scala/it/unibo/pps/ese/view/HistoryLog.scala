package it.unibo.pps.ese.view

case class HistoryLog(
  extinctSpecies:Seq[String],//Species extincted in this era
  mutantAlleles:Seq[String],//Sequence of Mutant Alleles Appeared in this era
  bornRegistry:Map[String,Long],//Species->Babies born in this era
  deadRegistry:Map[String,Long],//Species->Dead in this era
  couplingRegistry:Map[String,Long],//Species->Entities copulated in this era
)
