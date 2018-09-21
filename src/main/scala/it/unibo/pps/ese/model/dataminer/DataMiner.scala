package it.unibo.pps.ese.model.dataminer

import scala.collection.mutable

sealed trait DataMiner {

  def startEra: Era
  def lastEra: Era

  def populationTrend(): Seq[(Long, Long)]
  def simulationSpecies(): Seq[Species]
  def aliveSpecies(era: Era): Seq[Species]

  def extinctSpecies(): Seq[Species]
  def extinctSpecies(era: Era): Seq[Species]

  def aliveCount: Long
  def aliveCount(era: Era): Long
  def aliveCount(species: Species): Long
  def aliveCount(species: Species, era: Era): Long

  def deadCount: Long
  def deadCount(era: Era): Long
  def deadCount(species: Species): Long
  def deadCount(species: Species, era: Era): Long

  def bornCount: Long
  def bornCount(era: Era): Long
  def bornCount(species: Species): Long
  def bornCount(species: Species, era: Era): Long

  def couplingCount: Long
  def couplingCount(era: Era): Long
  def couplingCount(species: Species): Long
  def couplingCount(species: Species, era: Era): Long

  def mutantAlleles: Seq[String]
  def mutantAlleles(era: Era): Seq[String]
  def mutantAlleles(species: Species): Seq[String]
  def mutantAlleles(species: Species, era: Era): Seq[String]
}
object DataMiner {
  def apply(repository: ReadOnlyEntityRepository): DataMiner = new BaseDataMiner(repository)

  private class BaseDataMiner(repository: ReadOnlyEntityRepository) extends DataMiner {

    private[this] val _dataRepository = repository

    //DA VALUTARNE L'USO
    private def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
      override def apply(key: I): O = self.synchronized(getOrElseUpdate(key, f(key)))
    }

    private implicit class PimpedSeq[A](seq: Seq[A]) {
      def filterIf(condition: () => Boolean)(filter: A => Boolean): Seq[A] =
        if (condition()) seq.filter(filter) else seq
    }

    private def filterByEra(log: EntityLog, era: Era): Boolean =
      log.dynamicData.exists(y => y._1 == era)

    private def entitiesInEraBySpecies(species: Species, era: Era): Seq[EntityId] =
      _dataRepository.entitiesInEra(era).filter(x => x.structuralData.species == species).map(x => x.id)

    private def animalsDynamicData(era: Era, species: Species = ""): Seq[AnimalDynamicData] =
      _dataRepository
        .entitiesInEra(era)
        .collect {
          case b: EntityTimedRecord
            if b.dynamicData.isInstanceOf[AnimalDynamicData] && (species == "" || b.structuralData.species == species)
          => b.dynamicData.asInstanceOf[AnimalDynamicData]}

    override def startEra: Long =
      _dataRepository.getAllDynamicLogs()
        .map(x => x.dynamicData.map(y => y._1).min)
        .min

    override def lastEra: Long =
      _dataRepository.getAllDynamicLogs()
        .map(x => x.dynamicData.map(y => y._1).max)
        .max

    override def populationTrend(): Seq[(Long, Long)] =
      (startEra to lastEra)
        .map(i => (i, _dataRepository.entitiesInEra(i).size.toLong))

    override def simulationSpecies(): Seq[Species] =
      _dataRepository.getAllDynamicLogs().map(x => x.structuralData.species).distinct

    override def aliveSpecies(era: Era): Seq[Species] = {

      require(era >= startEra && era <= lastEra)

      _dataRepository.getAllDynamicLogs()
        .filter(x => filterByEra(x, era))
        .map(x => x.structuralData.species).distinct
    }

    override def extinctSpecies(era: Era): Seq[Species] = {

      require(era >= startEra && era <= lastEra)

      if (era == startEra) Seq.empty
      else aliveSpecies(era) diff aliveSpecies(era - 1)

    }

    override def extinctSpecies(): Seq[Species] =
      (startEra to lastEra).map(x => extinctSpecies(x)).fold(Seq empty)(_++_)

    override def aliveCount: Long =
      _dataRepository getAllDynamicLogs() size

    override def aliveCount(species: Species): Long =
      _dataRepository getAllDynamicLogs() count (x => x.structuralData.species == species)

    override def aliveCount(era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      _dataRepository entitiesInEra era size
    }

    override def aliveCount(species: Species, era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      entitiesInEraBySpecies(species, era) size
    }

    override def deadCount: Long = (startEra to lastEra).map(x => deadCount(x)).sum

    override def deadCount(species: Species): Long = (startEra to lastEra).map(x => deadCount(species, x)).sum

    override def deadCount(era: Era): Long = {

      require(era >= startEra && era <= lastEra)

      if (era == startEra) 0
      else (_dataRepository.entitiesInEra(era - 1) filterNot (_dataRepository.entitiesInEra(era) contains)) size
    }

    override def deadCount(species: Species, era: Era): Long = {

      require(era >= startEra && era <= lastEra)

      if (era == startEra) 0
      else (entitiesInEraBySpecies(species, era - 1) filterNot (entitiesInEraBySpecies(species, era) contains)) size
    }

    override def bornCount: Long = (startEra to lastEra).map(x => bornCount(x)).sum

    override def bornCount(species: Species): Long = (startEra to lastEra).map(x => bornCount(species, x)).sum

    override def bornCount(era: Era): Long = {

      require(era >= startEra && era <= lastEra)

      if (era == startEra) _dataRepository entitiesInEra era size
      else (_dataRepository.entitiesInEra(era) filterNot (_dataRepository.entitiesInEra(era - 1) contains)) size
    }

    override def bornCount(species: Species, era: Era): Long = {

      require(era >= startEra && era <= lastEra)

      if (era == startEra) entitiesInEraBySpecies(species, era) size
      else (entitiesInEraBySpecies(species, era) filterNot (entitiesInEraBySpecies(species, era - 1) contains)) size
    }

    override def couplingCount: Long = (startEra to lastEra).map(x => couplingCount(x)).sum

    override def couplingCount(species: Species): Long = (startEra to lastEra).map(x => couplingCount(species, x)).sum

    override def couplingCount(era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      animalsDynamicData(era).foldLeft(0)(_ + _.coupling.size)
    }

    override def couplingCount(species: Species, era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      animalsDynamicData(era, species).foldLeft(0)(_ + _.coupling.size)
    }

    override def mutantAlleles: Seq[String] =
      (startEra to lastEra).map(x => mutantAlleles(x)).foldLeft(Seq[String]())(_ ++ _)

    override def mutantAlleles(species: Species): Seq[String] =
      (startEra to lastEra).map(x => mutantAlleles(species, x)).foldLeft(Seq[String]())(_ ++ _)

    override def mutantAlleles(era: Era): Seq[String] = {
      require(era >= startEra && era <= lastEra)
      animalsDynamicData(era).foldLeft(Seq[String]())(_ ++ _.producedMutantGenes)
    }

    override def mutantAlleles(species: Species, era: Era): Seq[String] = {
      require(era >= startEra && era <= lastEra)
      animalsDynamicData(era, species).foldLeft(Seq[String]())(_ ++ _.producedMutantGenes)
    }
  }
}
