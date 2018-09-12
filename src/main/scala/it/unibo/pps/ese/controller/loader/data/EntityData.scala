package it.unibo.pps.ese.controller.loader.data

trait EntityData {
  def getName: Option[String]
  def getGeneLength: Option[Int]
  def getAlleleLength: Option[Int]
  def getReign: Option[String]
}

trait FullEntityData extends EntityData {
  def name: String = getName.getOrElse(throw new IllegalStateException())
  def geneLength: Int = getGeneLength.getOrElse(throw new IllegalStateException())
  def alleleLength: Int = getAlleleLength.getOrElse(throw new IllegalStateException())
  def reign: String = getReign.getOrElse(throw new IllegalStateException())
}
