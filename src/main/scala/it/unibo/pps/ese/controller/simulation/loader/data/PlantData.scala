package it.unibo.pps.ese.controller.simulation.loader.data

/** Trait that defines plant's data. All non-mandatory fields can be not filled, so are represented by an option*/
trait PartialPlantData extends EntityData {
  /**Plant's height*/
  val getHeight: Option[Double]
  /**Plant's nutritional value*/
  val getNutritionalValue: Option[Double]
  /**Plant's hardness*/
  val getHardness: Option[Double]
}

/**Rich trait that defines complete plant's data, where all non-mandatory fields must be filled. Trait can be directly
  * mixed with a[[it.unibo.pps.ese.controller.simulation.loader.data.PartialPlantData]]
  */
trait CompletePlantData extends PartialPlantData with FullEntityData {
  /**
    * @throws IllegalStateException if property is not set
    * @return Plant's height
    */
  @throws[IllegalStateException]
  def height: Double = getHeight.getOrElse(throw new IllegalStateException())
  /**
    * @throws IllegalStateException if propersty is not set
    * @return Plant's nutritional value
    */
  @throws[IllegalStateException]
  def nutritionalValue: Double = getNutritionalValue.getOrElse(throw new IllegalStateException())
  /**
    * @throws IllegalStateException if property is not set
    * @return Plant's hardness
    */
  @throws[IllegalStateException]
  def hardness: Double = getHardness.getOrElse(throw new IllegalStateException())
}
