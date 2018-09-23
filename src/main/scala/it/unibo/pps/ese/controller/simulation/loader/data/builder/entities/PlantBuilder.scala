package it.unibo.pps.ese.controller.simulation.loader.data.builder.entities

import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.GenericBuilder
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.EntityStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

trait PlantBuilder [T <: EntityStatus] extends EntityBuilder[T] with GenericBuilder[T, FullPlant, PartialPlantData, CompletePlantData] {
  override type RET[A <: T] = PlantBuilder[A]
  def setHeight(height: Double): PlantBuilder[T with PlantWithHeight]
  def setNutritionalValue(nutritionalValue: Double): PlantBuilder[T with PlantWithNutritionalValue]
  def setHardness(hardness: Double): PlantBuilder[T with PlantWithHardness]
}

object PlantBuilder {

  def apply(): PlantBuilder[EmptyEntity] =
    new PlantBuilderImpl[EmptyEntity](None, None, None, None, None, None, None)

  private class PlantBuilderImpl[T <: EntityStatus] (height: Option[Double],
                                                    nutritionalValue: Option[Double],
                                                    hardness: Option[Double],
                                                    name: Option[String],
                                                    geneLength: Option[Int],
                                                    alleleLength: Option[Int],
                                                    reign: Option[String])
                                                   (implicit val status: TypeTag[T])
    extends EntityBuilderImpl[T](name, geneLength, alleleLength, reign) with PlantBuilder[T] {


    def setHeight(height: Double): PlantBuilder[T with PlantWithHeight] =
      new PlantBuilderImpl(Some(height), nutritionalValue, hardness,name, geneLength,
        alleleLength, reign)

    def setNutritionalValue(nutritionalValue: Double): PlantBuilder[T with PlantWithNutritionalValue] =
      new PlantBuilderImpl(height, Some(nutritionalValue), hardness, name, geneLength,
        alleleLength, reign)

    def setHardness(hardness: Double): PlantBuilder[T with PlantWithHardness] =
      new PlantBuilderImpl(height, nutritionalValue, Some(hardness), name, geneLength,
        alleleLength, reign)

    def tryCompleteBuild(): Try[CompletePlantData] = {
      status.tpe match {
        case t if t <:< typeOf[FullPlant] =>
          Success(new PlantDataImpl(height, nutritionalValue, hardness, name.get, geneLength,
            alleleLength, reign) with CompletePlantData)
        case _ =>
          Failure(CompleteBuildException("Plant: " + name + " | All properties must be set"))
      }
    }

    def buildComplete(implicit ev: T =:= FullPlant, st: TypeTag[T]): CompletePlantData = {
      new PlantDataImpl(height, nutritionalValue, hardness, name.get, geneLength,
        alleleLength, reign) with CompletePlantData
    }

    def build(): PartialPlantData = {
      //require(status.tpe <:< st.tpe)
      require(status.tpe <:< typeOf[ValidEntity])
      tryCompleteBuild match {
        case Success(value) =>
          value
        case Failure(_) =>
          PlantDataImpl(height, nutritionalValue, hardness, name.get, geneLength,
            alleleLength, reign)
      }
    }

    override def newInstance[NT <: EntityStatus](name: Option[String], geneLength: Option[Int], alleleLength: Option[Int],
                                                 reign: Option[String])(implicit tt: universe.TypeTag[NT]): PlantBuilderImpl[NT] =
      new PlantBuilderImpl[NT](height, nutritionalValue, hardness, name, geneLength,
        alleleLength, reign)

  }

  private case class PlantDataImpl(getHeight: Option[Double],
                                   getNutritionalValue: Option[Double],
                                   getHardness: Option[Double],
                                   name: String,
                                   getGeneLength: Option[Int],
                                   getAlleleLength: Option[Int],
                                   getReign: Option[String]) extends PartialPlantData
}