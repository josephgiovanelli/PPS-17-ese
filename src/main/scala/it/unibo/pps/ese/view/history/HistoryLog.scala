package it.unibo.pps.ese.view.history

sealed trait LogType
case object BornLog extends LogType
case object DeadLog extends LogType
case object MutationLog extends LogType
case object CouplingLog extends LogType
case object ExtinctionLog extends LogType
case object PopulationLog extends LogType
case object GenerationLog extends LogType
case object MostPopulousLog extends LogType

case class Log(logText:String,logType:LogType)

case class HistoryLog(
  extinctSpecies:Seq[String],//Species extincted in this era
  mutantAlleles:Seq[String],//Sequence of Mutant Alleles Appeared in this era
  bornRegistry:Map[String,Long],//Species->Babies born in this era
  deadRegistry:Map[String,Long],//Species->Dead in this era
  couplingRegistry:Map[String,Long],//Species->Entities copulated in this era
)
object LogConversions{
  implicit class ConvertibleLog(historyLog: HistoryLog){
    def deadLogs:List[Log] = {
      historyLog.deadRegistry.map{case (species,dead)=>
          Log(dead+" entities of the "+species+" species are dead",DeadLog)
      }.toList
    }
    def bornLogs:List[Log] = {
      historyLog.bornRegistry.map{case (species,born)=>
        Log(born+" entities of the "+species+" species are born",BornLog)
      }.toList
    }
    def couplingLogs:List[Log] = {
      historyLog.couplingRegistry.map{case (species,coupling)=>
        Log(coupling+" entities of the "+species+" species have coupled",CouplingLog)
      }.toList
    }
    def extinctionLogs:List[Log] = {
      historyLog.extinctSpecies.map(species=>Log("The "+species+" species is now extinct",ExtinctionLog)).toList
    }
    def mutantLogs:List[Log] = {
      historyLog.mutantAlleles.map(allele=>Log("Mutant alleles appeared for the gene: "+allele,MutationLog)).toList
    }

    def allLogs:List[Log] = {
      bornLogs:::
      deadLogs:::
      couplingLogs:::
      extinctionLogs:::
      mutantLogs
    }
    def allLogsWithAggregation(historyAggregator: HistoryAggregator):List[Log] = {
      allLogs ::: historyAggregator.processLog(historyLog)
    }
  }
}

