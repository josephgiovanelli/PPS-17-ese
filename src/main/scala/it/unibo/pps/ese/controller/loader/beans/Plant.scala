package it.unibo.pps.ese.controller.loader.beans

import it.unibo.pps.ese.controller.loader.data.PlantData

case class Plant(override val name: String,
                 override val geneLength: Int,
                 override val alleleLength: Int,
                 override val reign: String,
                 height: Double,
                 attractiveness: Double,
                 hardness: Double,
                 nutritionalValue: Double,
                 availability: Double) extends PlantData {
  //TODO
  override def getName: Option[String] = None

  override def getGeneLength: Option[Int] = None

  override def getAlleleLength: Option[Int] = None

  override def getReign: Option[String] = None
}
