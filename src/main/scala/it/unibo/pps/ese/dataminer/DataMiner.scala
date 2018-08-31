package it.unibo.pps.ese.dataminer

sealed trait DataMiner {
  def populationTrend(): Seq[Int]
}
object DataMiner {
  def apply(repository: ReadOnlyEntityRepository): DataMiner = new BaseDataMiner(repository)

  private class BaseDataMiner(repository: ReadOnlyEntityRepository) extends DataMiner {

    private[this] val _dataRepository = repository

    override def populationTrend(): Seq[Int] =
      (1 to _dataRepository.getAllDynamicLogs().map(x => x.dynamicData.map(y => y._1).max.toInt).max)
        .map(i => _dataRepository.entitiesInEra(i).size)
  }
}
