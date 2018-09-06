package it.unibo.pps.ese.dataminer

sealed trait DataMiner {
  def populationTrend(): Seq[Int]
  def worldSpecies(): Seq[Species]
}
object DataMiner {
  def apply(repository: ReadOnlyEntityRepository): DataMiner = new BaseDataMiner(repository)

  private class BaseDataMiner(repository: ReadOnlyEntityRepository) extends DataMiner {

    private[this] val _dataRepository = repository

    override def worldSpecies(): Seq[Species] =
      _dataRepository.getAllDynamicLogs().map(x => x.structuralData.species).distinct

    override def populationTrend(): Seq[Int] =
      (0 to _dataRepository.getAllDynamicLogs().map(x => x.dynamicData.map(y => y._1).max.toInt).max)
        .map(i => _dataRepository.entitiesInEra(i).size)
  }
}
