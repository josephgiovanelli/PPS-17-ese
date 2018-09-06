package it.unibo.pps.ese.dataminer

sealed trait DataSaver {
  def saveData(path: String, data: Seq[EntityLog]): String
  def loadData(path: String): Seq[EntityLog]
}
object DataSaver {
  def apply(): DataSaver = new BaseDataSaver()

  private class BaseDataSaver() extends DataSaver {

    import org.json4s._
    import org.json4s.jackson.Serialization.{write, read}

    implicit val formats = DefaultFormats

    override def saveData(path: String, data: Seq[EntityLog]): String = write(data)

    override def loadData(serializedData: String): Seq[EntityLog] = read[Seq[EntityLog]](serializedData)
  }
}
