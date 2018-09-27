package it.unibo.pps.ese.controller.simulation.loader.data.builder.entities

import it.unibo.pps.ese.controller.simulation.loader.Reigns
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.{BaseGenericBuilder, GenericBuilder, ValidStatusGenericBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.EntityStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableInsideRange._

/** Builder that can build a PartialPlantData as partial data instance and a CompletePlantData as complete data
  * instance
  *
  * @tparam T Builder's current status
  */
trait PlantBuilder [T <: EntityStatus] extends EntityBuilder[T] with GenericBuilder[T, FullPlant, PartialPlantData, CompletePlantData] {
  override type RET[A <: T] = PlantBuilder[A]
  /** Set plant's height
    *
    * @param height Plant's height
    * @return New builder with updated param and status
    */
  def setHeight(height: Double): PlantBuilder[T with PlantWithHeight]
  /** Set plant's nutritional value
    *
    * @param nutritionalValue Plant's nutritional value
    * @return New builder with updated param and status
    */
  def setNutritionalValue(nutritionalValue: Double): PlantBuilder[T with PlantWithNutritionalValue]
  /** Set plant's hardness
    *
    * @param hardness Plant's hardness
    * @return New builder with updated param and status
    */
  def setHardness(hardness: Double): PlantBuilder[T with PlantWithHardness]
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.PlantBuilder]]*/
object PlantBuilder {

  /** Create a new empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.PlantBuilder]]
    *
    * @return An empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.PlantBuilder]]
    */
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
      with BaseGenericBuilder[T , FullPlant, PartialPlantData, CompletePlantData]
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
      if(!reign.contains(Reigns.ANIMALS.code))
        exception = exception ++: InvalidParamValueBuildException("Animal: " + name.getOrElse(""), "reign", reign)
      if(!height.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Plant " + name.getOrElse(""), "height", height)
      if(!nutritionalValue.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Plant " + name.getOrElse(""), "nutritionalValue", nutritionalValue)
      if(!hardness.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Plant " + name.getOrElse(""), "hardness", hardness)
      exception
    }

    override protected def buildPartialInstance(): PartialPlantData =
      new PlantDataImpl(height, nutritionalValue, hardness, name.get, geneLength, alleleLength, reign)
  }

  private[this] class PlantDataImpl(_getHeight: Option[Double],
                                    _getNutritionalValue: Option[Double],
                                    _getHardness: Option[Double],
                                    _name: String,
                                    _getGeneLength: Option[Int],
                                    _getAlleleLength: Option[Int],
                                    _getReign: Option[String])
    extends EntityDataImpl(_name, _getGeneLength, _getAlleleLength, _getReign) with PartialPlantData {

    import it.unibo.pps.ese.controller.simulation.loader.data.builder.BuildersValidationImplicits._

    override val getHeight: Option[Double] = _getHeight.normalize()
    override val getNutritionalValue: Option[Double] = _getNutritionalValue.normalize()
    override val getHardness: Option[Double] = _getHardness.normalize()
  }
}