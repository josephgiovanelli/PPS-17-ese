package it.unibo.pps.ese.controller.loader.data

trait PartialAlleleData {
  def getGene: Option[String]
  def getId: Option[String]
  def getDominance: Option[Double]
  def getConsume: Option[Double]
  def getProbability: Option[Double]
  def getEffect: Option[Map[String, Double]]
}

trait CompleteAlleleData extends PartialAlleleData {
  def gene: String = getGene.getOrElse(throw new IllegalStateException)
  def id: String = getId.getOrElse(throw new IllegalStateException)
  def dominance: Double = getDominance.getOrElse(throw new IllegalStateException)
  def consume: Double = getConsume.getOrElse(throw new IllegalStateException)
  def probability: Double = getProbability.getOrElse(throw new IllegalStateException)
  def effect: Map[String, Double] = getEffect.getOrElse(throw new IllegalStateException)
}
