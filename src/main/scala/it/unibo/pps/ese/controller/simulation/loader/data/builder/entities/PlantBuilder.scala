package it.unibo.pps.ese.controller.simulation.loader.data.builder.entities

import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.{BaseBuildableGenericBuilder, GenericBuilder, ValidStatusGenericBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.EntityStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableInsideRange._

trait PlantBuilder [T <: EntityStatus] extends EntityBuilder[T] with GenericBuilder[T, FullPlant, PartialPlantData, CompletePlantData] {
  override type RET[A <: T] = PlantBuilder[A]
  def setHeight(height: Double): PlantBuilder[T with PlantWithHeight]
  def setNutritionalValue(nutritionalValue: Double): PlantBuilder[T with PlantWithNutritionalValue]
  def setHardness(hardness: Double): PlantBuilder[T with PlantWithHardness]
}

object PlantBuilder {

  def apply(): PlantBuilder[EmptyEntity] =
    new PlantBuilderImpl[EmptyEntity](None, None, None, None, None, None, None)

  private[this] class PlantBuilderImpl[T <: EntityStatus] (height: Option[Double],
                                                    nutritionalValue: Option[Double],
                                                    hardness: Option[Double],
                                                    name: Option[String],
                                                    geneLength: Option[Int],
                                                    alleleLength: Option[Int],
                                                    reign: Option[String])
                                                   (implicit val status: TypeTag[T])
    extends EntityBuilderImpl[T](name, geneLength, alleleLength, reign) with PlantBuilder[T]
      with BaseBuildableGenericBuilder[T , FullPlant, PartialPlantData, CompletePlantData]
      with ValidStatusGenericBuilder[T , FullPlant, PartialPlantData, CompletePlantData, ValidEntity]{


    def setHeight(height: Double): PlantBuilder[T with PlantWithHeight] =
      new PlantBuilderImpl(Some(height), nutritionalValue, hardness,name, geneLength,
        alleleLength, reign)

    def setNutritionalValue(nutritionalValue: Double): PlantBuilder[T with PlantWithNutritionalValue] =
      new PlantBuilderImpl(height, Some(nutritionalValue), hardness, name, geneLength,
        alleleLength, reign)

    def setHardness(hardness: Double): PlantBuilder[T with PlantWithHardness] =
      new PlantBuilderImpl(height, nutritionalValue, Some(hardness), name, geneLength,
        alleleLength, reign)

    override def newInstance[NT <: EntityStatus](name: Option[String], geneLength: Option[Int], alleleLength: Option[Int],
                                                 reign: Option[String])(implicit tt: universe.TypeTag[NT]): PlantBuilder[NT] =
      new PlantBuilderImpl[NT](height, nutritionalValue, hardness, name, geneLength,
        alleleLength, reign)

    def tryCompleteBuild(): Try[CompletePlantData] = {
      status.tpe match {
        case t if t <:< typeOf[FullPlant] =>
          val exception = checkProperties
          if(exception.isEmpty) {
            Success(new PlantDataImpl(height, nutritionalValue, hardness, name.get, geneLength,
              alleleLength, reign) with CompletePlantData)
          } else {
            Failure(exception.get)
          }
        case _ =>
          Failure(CompleteBuildException("Plant: " + name + " | All properties must be set"))
      }
    }

    override def checkProperties: Option[CompleteBuildException] = {
      var exception = super.checkProperties
      if(!height.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Plant " + name.getOrElse(""), "height", height)
      if(!nutritionalValue.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Plant " + name.getOrElse(""), "nutritionalValue", nutritionalValue)
      if(!hardness.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Plant " + name.getOrElse(""), "hardness", hardness)
      exception
    }

    override protected def buildPartialInstance(): PartialPlantData =
      PlantDataImpl(height, nutritionalValue, hardness, name.get, geneLength, alleleLength, reign)
  }
  private[this] case class PlantDataImpl(getHeight: Option[Double],
                                   getNutritionalValue: Option[Double],
                                   getHardness: Option[Double],
                                   name: String,
                                   getGeneLength: Option[Int],
                                   getAlleleLength: Option[Int],
                                   getReign: Option[String]) extends PartialPlantData
}