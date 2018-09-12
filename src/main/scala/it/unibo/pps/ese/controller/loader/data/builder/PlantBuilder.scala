package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.beans.Plant
import it.unibo.pps.ese.controller.loader.data.AnimalData.{AnimalDataImpl, CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus.FullAnimal
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.PlantBuilder.PlantStatus
import it.unibo.pps.ese.controller.loader.data.builder.PlantBuilder.PlantStatus._

import scala.reflect.runtime.universe._

trait PlantBuilder [T <: PlantStatus]{
  def setHeight(height: Double): PlantBuilder[T with PlantWithHeight]
  def setNutritionalValue(nutritionalValue: Double): PlantBuilder[T with PlantWithNutritionalValue]
  def setAttractiveness(attractiveness: Double): PlantBuilder[T with PlantWithAttractiveness]
  def setHardness(hardness: Double): PlantBuilder[T with PlantWithHardness]
  def setAvailability(availability: Double): PlantBuilder[T with PlantWithAvailability]
  def setName(name: String): PlantBuilder[T with PlantWithName]
  def setGeneLength(geneLength: Int): PlantBuilder[T with PlantWithGeneLength]
  def setAlleleLength(alleleLength: Int): PlantBuilder[T with PlantWithAlleleLength]
  def setReign(reign: String): PlantBuilder[T with PlantWithReign]
  def setInfo(plant: Plant): PlantBuilder[T with FullPlant]
  def buildComplete(implicit ev: T =:= FullPlant): CompletePlantData
  def build(): PartialPlantData
}

object PlantBuilder {

  def apply(): PlantBuilder[EmptyPlant] =
    new PlantBuilderImpl[EmptyPlant](None, None, None, None, None, None, None, None, None)

  private class PlantBuilderImpl[T <: PlantStatus] (height: Option[Double],
                                                    nutritionalValue: Option[Double],
                                                    attractiveness: Option[Double],
                                                    hardness: Option[Double],
                                                    availability: Option[Double],
                                                    name: Option[String],
                                                    geneLength: Option[Int],
                                                    alleleLength: Option[Int],
                                                    reign: Option[String])
                                                   (implicit val status: TypeTag[T]) extends PlantBuilder[T] {

    def setHeight(height: Double): PlantBuilder[T with PlantWithHeight] =
      new PlantBuilderImpl(Some(height), nutritionalValue, attractiveness, hardness, availability, name, geneLength,
        alleleLength, reign)

    def setNutritionalValue(nutritionalValue: Double): PlantBuilder[T with PlantWithNutritionalValue] =
      new PlantBuilderImpl(height, Some(nutritionalValue), attractiveness, hardness, availability, name, geneLength,
        alleleLength, reign)

    def setAttractiveness(attractiveness: Double): PlantBuilder[T with PlantWithAttractiveness] =
      new PlantBuilderImpl(height, nutritionalValue, Some(attractiveness), hardness, availability, name, geneLength,
        alleleLength, reign)

    def setHardness(hardness: Double): PlantBuilder[T with PlantWithHardness] =
      new PlantBuilderImpl(height, nutritionalValue, attractiveness, Some(hardness), availability, name, geneLength,
        alleleLength, reign)

    def setAvailability(availability: Double): PlantBuilder[T with PlantWithAvailability] =
      new PlantBuilderImpl(height, nutritionalValue, attractiveness, hardness, Some(availability), name, geneLength,
        alleleLength, reign)

    def setName(name: String): PlantBuilder[T with PlantWithName] =
      new PlantBuilderImpl(height, nutritionalValue, attractiveness, hardness, availability, Some(name), geneLength,
        alleleLength, reign)

    def setGeneLength(geneLength: Int): PlantBuilder[T with PlantWithGeneLength] =
      new PlantBuilderImpl(height, nutritionalValue, attractiveness, hardness, availability, name, Some(geneLength),
        alleleLength, reign)

    def setAlleleLength(alleleLength: Int): PlantBuilder[T with PlantWithAlleleLength] =
      new PlantBuilderImpl(height, nutritionalValue, attractiveness, hardness, availability, name, geneLength,
        Some(alleleLength), reign)

    def setReign(reign: String): PlantBuilder[T with PlantWithReign] =
      new PlantBuilderImpl(height, nutritionalValue, attractiveness, hardness, availability, name, geneLength,
        alleleLength, Some(reign))

    def setInfo(plant: Plant): PlantBuilder[T with FullPlant] = {
      new PlantBuilderImpl(Some(plant.height), Some(plant.nutritionalValue), Some(plant.attractiveness),
        Some(plant.hardness), Some(plant.availability), Some(plant.name), Some(plant.geneLength),
        Some(plant.alleleLength), Some(plant.reign))
    }

    def buildComplete(implicit ev: T =:= FullPlant): CompletePlantData = {
      new PlantDataImpl(height, nutritionalValue, attractiveness, hardness, availability, name, geneLength,
        alleleLength, reign) with CompletePlantData
    }

    def build(): PartialPlantData = {
      //require(status.tpe <:< st.tpe)
      status.tpe match {
        case t if t <:< typeOf[FullPlant] =>
          new PlantDataImpl(height, nutritionalValue, attractiveness, hardness, availability, name, geneLength,
            alleleLength, reign) with CompletePlantData
        case _ =>
          PlantDataImpl(height, nutritionalValue, attractiveness, hardness, availability, name, geneLength,
            alleleLength, reign)
      }
    }
  }

  private case class PlantDataImpl(getHeight: Option[Double],
                                   getNutritionalValue: Option[Double],
                                   getAttractiveness: Option[Double],
                                   getHardness: Option[Double],
                                   getAvailability: Option[Double],
                                   getName: Option[String],
                                   getGeneLength: Option[Int],
                                   getAlleleLength: Option[Int],
                                   getReign: Option[String]) extends PartialPlantData

  sealed trait PlantStatus
  object PlantStatus {
    sealed trait EmptyPlant extends PlantStatus
    sealed trait PlantWithHeight extends PlantStatus
    sealed trait PlantWithNutritionalValue extends PlantStatus
    sealed trait PlantWithAttractiveness extends PlantStatus
    sealed trait PlantWithHardness extends PlantStatus
    sealed trait PlantWithAvailability extends PlantStatus
    sealed trait PlantWithName extends PlantStatus
    sealed trait PlantWithGeneLength extends PlantStatus
    sealed trait PlantWithAlleleLength extends PlantStatus
    sealed trait PlantWithReign extends PlantStatus

    type FullPlant = EmptyPlant with PlantWithHeight with PlantWithNutritionalValue with PlantWithAttractiveness with
      PlantWithHardness with PlantWithAvailability with PlantWithName with PlantWithGeneLength
      with PlantWithAlleleLength with PlantWithReign
  }
}