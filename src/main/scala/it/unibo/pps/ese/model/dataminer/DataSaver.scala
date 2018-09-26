package it.unibo.pps.ese.model.dataminer

import it.unibo.pps.ese.model.dataminer.datamodel.{EntityLog, EntityLogImpl}

/**
  * This trait contains APIs related to consolidated data loading and storing
  */
sealed trait DataSaver {

  /**
    * Serialize the input consolidated data
    * @param data The consolidated data to be serialized
    * @return The serialized data
    */
  def saveData(data: Seq[EntityLog]): String

  /**
    * Deserialize the input consolidated data
    * @param serializedData The consolidated data to be deserialized
    * @return The deserialized data
    */
  @deprecated("This method isn't actually working", "DataSaver 1.0")
  def loadData(serializedData: String): Seq[EntityLog]
}

object DataSaver {
  def apply(): DataSaver = new BaseDataSaver()

  /**
    * DataSaver implementation based on JSon format
    */
  private class BaseDataSaver() extends DataSaver {

    import org.json4s._
    import org.json4s.jackson.Serialization.{write, read}

    implicit private val formats: Formats = DefaultFormats

    override def saveData(data: Seq[EntityLog]): String = write(data)

    override def loadData(serializedData: String): Seq[EntityLog] = try {
      read[Seq[EntityLogImpl]](serializedData)
    } catch  {
      case e: Exception => throw e
    }
  }
}
