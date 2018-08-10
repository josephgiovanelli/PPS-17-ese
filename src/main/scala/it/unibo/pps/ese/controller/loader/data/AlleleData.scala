package it.unibo.pps.ese.controller.loader.data

trait AlleleData {
  def gene: String
  def id: String
  def dominance: Double
  def consume: Double
  def probability: Double
  def effect: Map[String, Double]
}
