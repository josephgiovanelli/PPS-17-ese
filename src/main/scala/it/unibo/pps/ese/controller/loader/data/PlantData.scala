package it.unibo.pps.ese.controller.loader.data

trait PartialPlantData extends EntityData {
  def getHeight: Option[Double]
  def getNutritionalValue: Option[Double]
  def getHardness: Option[Double]
  def getAvailability: Option[Double]
}

trait CompletePlantData extends PartialPlantData with FullEntityData {
  def height: Double = getHeight.getOrElse(throw new IllegalStateException())
  def nutritionalValue: Double = getNutritionalValue.getOrElse(throw new IllegalStateException())
  def hardness: Double = getHardness.getOrElse(throw new IllegalStateException())
  def availability: Double = getAvailability.getOrElse(throw new IllegalStateException())
}
