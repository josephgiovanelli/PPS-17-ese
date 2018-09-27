package it.unibo.pps.ese.model.dataminer

import java.util.concurrent.atomic.AtomicLong

import it.unibo.pps.ese.model.dataminer.DataModelSupport.{EntityId, Era, Species}
import it.unibo.pps.ese.model.dataminer.datamodel.{AnimalDynamicData, EntityLog, EntityTimedRecord, ReadOnlyEntityRepository}
import it.unibo.pps.ese.model.dataminer.utils.Memoizer

/**
  * This is a collection of function used to analyze the simulation consolidated data and produce statistics
  * from them
  */
sealed trait DataMiner {

  /**
    * Get the first era to which data refer
    * @return The era
    */
  def startEra: Era

  /**
    * Get the last era to which data refer
    * @return The era
    */
  def lastEra: Era

  /**
    * Get the simulation's population trend
    * @return The population trend
    */
  def populationTrend(): Seq[(Long, Long)]

  /**
    * Get a list of the species contained in the Simulation world, including the extinct ones
    * @return The species list
    */
  def simulationSpecies(): Seq[Species]

  /**
    * Get a list of the species alive in the selected Era in the Simulation world
    * @return The species list
    */
  def aliveSpecies(era: Era): Seq[Species]

  /**
    * Get a list of the extinct species
    * @return The species list
    */
  def extinctSpecies(): Seq[Species]

  /**
    * Get a list of extinct species in the selected era
    * @return The species list
    */
  def extinctSpecies(era: Era): Seq[Species]

  /**
    * Get the total number of living entities in the Simulation world
    * @return The entities count
    */
  def aliveCount: Long

  /**
    * Get the total number of living entities in the Simulation world in the selected era
    * @return The entities count
    */
  def aliveCount(era: Era): Long

  /**
    * Get the total number of living entities in the Simulation world of the selected Species
    * @return The entities count
    */
  def aliveCount(species: Species): Long

  /**
    * Get the total number of living entities in the Simulation world of the selected Species in the selected Era
    * @return The entities count
    */
  def aliveCount(species: Species, era: Era): Long

  /**
    * Get the total number of entities died in the Simulation world
    * @return The entities count
    */
  def deadCount: Long

  /**
    * Get the total number of entities died in the Simulation world in the selected era
    * @return The entities count
    */
  def deadCount(era: Era): Long

  /**
    * Get the total number of entities died in the Simulation world of the selected Species
    * @return The entities count
    */
  def deadCount(species: Species): Long

  /**
    * Get the total number of entities died in the Simulation world of the selected Species in the selected Era
    * @return The entities count
    */
  def deadCount(species: Species, era: Era): Long

  /**
    * Get the total number of entities born in the Simulation world
    * @return The entities count
    */
  def bornCount: Long

  /**
    * Get the total number of entities born in the Simulation world in the selected era
    * @return The entities count
    */
  def bornCount(era: Era): Long

  /**
    * Get the total number of entities born in the Simulation world of the selected Species
    * @return The entities count
    */
  def bornCount(species: Species): Long

  /**
    * Get the total number of entities born in the Simulation world of the selected Species in the selected Era
    * @return The entities count
    */
  def bornCount(species: Species, era: Era): Long

  /**
    * Get the total number of couplings in the Simulation world
    * @return The couplings count
    */
  def couplingCount: Long

  /**
    * Get the total number of couplings in the Simulation world in the selected era
    * @return The couplings count
    */
  def couplingCount(era: Era): Long

  /**
    * Get the total number of couplings in the Simulation world of the selected Species
    * @return The couplings count
    */
  def couplingCount(species: Species): Long

  /**
    * Get the total number of couplings in the Simulation world of the selected Species in the selected Era
    * @return The couplings count
    */
  def couplingCount(species: Species, era: Era): Long

  /**
    * Get the total number of manifested mutant alleles in the Simulation world
    * @return The mutations count
    */
  def mutantAlleles: Seq[String]

  /**
    * Get the total number of manifested mutant alleles in the Simulation world in the selected Era
    * @return The mutations count
    */
  def mutantAlleles(era: Era): Seq[String]

  /**
    * Get the total number of manifested mutant alleles in the Simulation world of the selected Species
    * @return The mutations count
    */
  def mutantAlleles(species: Species): Seq[String]

  /**
    * Get the total number of manifested mutant alleles in the Simulation world of the selected Species
    * in the selected Era
    * @return The mutations count
    */
  def mutantAlleles(species: Species, era: Era): Seq[String]
}
object DataMiner {

  /**
    * @param repository The data source
    * @return A DataMiner instance
    */
  def apply(repository: ReadOnlyEntityRepository): DataMiner = new MemoizedDataMiner(repository)

  /**
    * Data miner implementation based on the use of memoization to optimize queries response time
    */
  private class MemoizedDataMiner(repository: ReadOnlyEntityRepository) extends DataMiner with Memoizer {

    private[this] val _dataRepository = repository
    private[this] val _lastEra = new AtomicLong(0)

    repository attachNewDataListener(era => this synchronized { _lastEra set era})

    private val _entitiesInEraBySpecies: ((Species, Era)) => Seq[EntityId] = memoize {
      input =>
        _dataRepository.entitiesInEra(input._2).filter(x => x.structuralData.species == input._1).map(x => x.id)
    }

    private val _aliveSpecies: Era => Seq[Species] = memoize {
      era =>_dataRepository.getAllDynamicLogs()
        .filter(x => filterByEra(x, era))
        .map(x => x.structuralData.species).distinct
    }

    private val _extinctSpecies: Era => Seq[Species] = memoize {
      era =>
        if (era == startEra) Seq.empty
        else aliveSpecies(era - 1) filterNot (aliveSpecies(era).contains(_))
    }

    private val _aliveCount: Era => Long = memoize {
      era =>
        _dataRepository entitiesInEra era size
    }

    private val _deadCountEra: Era => Long = memoize {
      era =>
        if (era == startEra) 0
        else (_dataRepository.entitiesInEra(era - 1) filterNot (_dataRepository.entitiesInEra(era) contains)) size
    }

    private val _deadCountEraSpecies: ((Species, Era)) => Long = memoize {
      input =>
        if (input._2 == startEra) 0
        else (entitiesInEraBySpecies(input._1, input._2 - 1) filterNot (entitiesInEraBySpecies(input._1, input._2) contains)) size
    }

    private val _bornCountEra: Era => Long = memoize {
      era =>
        if (era == startEra) _dataRepository entitiesInEra era size
        else (_dataRepository.entitiesInEra(era) filterNot (_dataRepository.entitiesInEra(era - 1) contains)) size
    }

    private val _bornCountEraSpecies: ((Species, Era)) => Long = memoize {
      input =>
        if (input._2 == startEra) entitiesInEraBySpecies(input._1, input._2) size
        else (entitiesInEraBySpecies(input._1, input._2) filterNot (entitiesInEraBySpecies(input._1, input._2 - 1) contains)) size
    }

    private val _couplingCountEra: Era => Long = memoize {
      era => animalsDynamicData(era).foldLeft(0)(_ + _.coupling.size)
    }

    private val _couplingCountEraSpecies: ((Species, Era)) => Long = memoize {
      input => animalsDynamicData(input._2, input._1).foldLeft(0)(_ + _.coupling.size)
    }

    private val _mutantAllelesEra: Era => Seq[String] = memoize {
      era => animalsDynamicData(era).foldLeft(Seq[String]())(_ ++ _.producedMutantGenes)
    }

    private val _mutantAllelesEraSpecies: ((Species, Era)) => Seq[String] = memoize {
      input => animalsDynamicData(input._2, input._1).foldLeft(Seq[String]())(_ ++ _.producedMutantGenes)
    }

    private def filterByEra(log: EntityLog, era: Era): Boolean =
      log.dynamicData.exists(y => y._1 == era)

    private def entitiesInEraBySpecies(species: Species, era: Era): Seq[EntityId] =
      _entitiesInEraBySpecies(species, era)

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

    override def lastEra: Long = _lastEra get()

    override def populationTrend(): Seq[(Long, Long)] =
      (startEra to lastEra)
        .map(i => (i, _dataRepository.entitiesInEra(i).size.toLong))

    override def simulationSpecies(): Seq[Species] =
      _dataRepository.getAllDynamicLogs().map(x => x.structuralData.species).distinct

    override def aliveSpecies(era: Era): Seq[Species] = {
      require(era >= startEra && era <= lastEra)
      _aliveSpecies(era)
    }

    override def extinctSpecies(era: Era): Seq[Species] = {
      require(era >= startEra && era <= lastEra)
      _extinctSpecies(era)
    }

    override def extinctSpecies(): Seq[Species] =
      (startEra to lastEra).map(x => extinctSpecies(x)).fold(Seq empty)(_++_)

    override def aliveCount: Long =
      _dataRepository getAllDynamicLogs() size

    override def aliveCount(species: Species): Long =
      _dataRepository getAllDynamicLogs() count (x => x.structuralData.species == species)

    override def aliveCount(era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      _aliveCount(era)
    }

    override def aliveCount(species: Species, era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      entitiesInEraBySpecies(species, era) size
    }

    override def deadCount: Long = (startEra to lastEra).map(x => deadCount(x)).sum

    override def deadCount(species: Species): Long = (startEra to lastEra).map(x => deadCount(species, x)).sum

    override def deadCount(era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      _deadCountEra(era)
    }

    override def deadCount(species: Species, era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      _deadCountEraSpecies(species, era)
    }

    override def bornCount: Long = (startEra to lastEra).map(x => bornCount(x)).sum

    override def bornCount(species: Species): Long = (startEra to lastEra).map(x => bornCount(species, x)).sum

    override def bornCount(era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      _bornCountEra(era)
    }

    override def bornCount(species: Species, era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      _bornCountEraSpecies(species, era)
    }

    override def couplingCount: Long = (startEra to lastEra).map(x => couplingCount(x)).sum

    override def couplingCount(species: Species): Long = (startEra to lastEra).map(x => couplingCount(species, x)).sum

    override def couplingCount(era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      _couplingCountEra(era)
    }

    override def couplingCount(species: Species, era: Era): Long = {
      require(era >= startEra && era <= lastEra)
      _couplingCountEraSpecies(species, era)
    }

    override def mutantAlleles: Seq[String] =
      (startEra to lastEra).map(x => mutantAlleles(x)).foldLeft(Seq[String]())(_ ++ _)

    override def mutantAlleles(species: Species): Seq[String] =
      (startEra to lastEra).map(x => mutantAlleles(species, x)).foldLeft(Seq[String]())(_ ++ _)

    override def mutantAlleles(era: Era): Seq[String] = {
      require(era >= startEra && era <= lastEra)
      _mutantAllelesEra(era)
    }

    override def mutantAlleles(species: Species, era: Era): Seq[String] = {
      require(era >= startEra && era <= lastEra)
      _mutantAllelesEraSpecies(species, era)
    }
  }
}
