package it.unibo.pps.ese.controller.loader.data

trait PlantData extends EntityData {
  def height: Double
  def nutritionalValue: Double
  def attractiveness: Double
  def hardness: Double
  def availability: Double
}
