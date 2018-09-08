package it.unibo.pps.ese.dataminer

sealed trait DataMiner {

  def startEra: Era
  def lastEra: Era

  def populationTrend(): Seq[Long]
  def simulationSpecies(): Seq[Species]
  def aliveSpecies(era: Era): Seq[Species]

  def extinctSpecies(): Seq[Species]
  def extinctSpecies(era: Era): Seq[Species]

  def deadCount: Long
  def deadCount(era: Era): Long
  def deadCount(species: Species): Long
  def deadCount(species: Species, era: Era): Long

  def bornCount: Long
  def bornCount(era: Era): Long
  def bornCount(species: Species): Long
  def bornCount(species: Species, era: Era): Long


  //def couplingCount(species: Species, era: Era = -1): Long
  //def mutantAlleles(era: Era = -1): Seq[String]
}
object DataMiner {
  def apply(repository: ReadOnlyEntityRepository): DataMiner = new BaseDataMiner(repository)

  private class BaseDataMiner(repository: ReadOnlyEntityRepository) extends DataMiner {

    private[this] val _dataRepository = repository

    private implicit class PimpedSeq[A](seq: Seq[A]) {
      def filterIf(condition: () => Boolean)(filter: A => Boolean): Seq[A] =
        if (condition()) seq.filter(filter) else seq
    }

    private def filterByEra(log: EntityLog, era: Era): Boolean =
      log.dynamicData.exists(y => y._1 == era)

    private def entitiesInEraBySpecies(species: Species, era: Era): Seq[EntityId] =
      _dataRepository.entitiesInEra(era).filter(x => x.structuralData.species == species).map(x => x.id)

    override def startEra: Long =
      _dataRepository.getAllDynamicLogs()
        .map(x => x.dynamicData.map(y => y._1).min)
        .min

    override def lastEra: Long =
      _dataRepository.getAllDynamicLogs()
        .map(x => x.dynamicData.map(y => y._1).max)
        .max

    override def populationTrend(): Seq[Long] =
      (startEra to lastEra)
        .map(i => _dataRepository.entitiesInEra(i).size.toLong)

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
  }
}
