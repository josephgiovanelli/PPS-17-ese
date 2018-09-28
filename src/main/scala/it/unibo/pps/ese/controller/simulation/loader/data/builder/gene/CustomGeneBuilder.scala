package it.unibo.pps.ese.controller.simulation.loader.data.builder.gene

import it.unibo.pps.ese.controller.simulation.loader.beans.PropertyInfo
import it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.{CustomGeneData, PartialAlleleData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.DefaultGeneBuilder.DefaultGeneDataImpl
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.GeneStatus._

import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._

/** Builder that can build a PartialCustomGeneData as partial data instance and a CompleteCustomGeneData as complete data
  * instance
  *
  * @tparam S Builder's current status
  */
trait CustomGeneBuilder[S <: GeneStatus] extends GeneBuilder[S] with GenericBuilder[S, CustomGene, PartialCustomGeneData, CompleteCustomGeneData] {
  type RET[A <: S] = CustomGeneBuilder[A]
  /** Set multiple builder's properties starting from a map containing properties' names as key and theirs info as value
    *
    * @param properties Custom gene properties
    * @return New builder with updated param and status
    */
  def setCustomProperties(properties: Map[String, PropertyInfo]): CustomGeneBuilder[S with GeneWithProperties with GeneWithConversionMap]
  /** Set gene's conversion map with a map containing properties as key and the respective conversion maps as value
    *
    * @param conversionMap Gene's conversion map
    * @return New builder with updated param and status
    */
  def setConversionMap(conversionMap: Map[String, Map[String, Double]]): CustomGeneBuilder[S with GeneWithConversionMap]
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.CustomGeneBuilder]]*/
object CustomGeneBuilder {

  /** Create a new empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.CustomGeneBuilder]]
    *
    * @return An empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.CustomGeneBuilder]]
    */
  def apply(): CustomGeneBuilder[EmptyGene] = new CustomGeneBuilderImpl[EmptyGene](None, None, Map(), Iterable(), Map())

  private[this] class CustomGeneBuilderImpl[T <: GeneStatus](_id: Option[String],
                                                             _name: Option[String],
                                                             _properties: Map[String, Class[_]],
                                                             _alleles: Iterable[AlleleBuilder[_]],
                                                             _conversionMap: Map[String, Map[String, Double]])
                                              (implicit val status: TypeTag[T])
    extends GenericGeneBuilderImpl[T](_id, _name, _properties, _alleles) with CustomGeneBuilder[T]
      with BaseGenericBuilder[T , CustomGene, PartialCustomGeneData, CompleteCustomGeneData]
      with ValidStatusGenericBuilder[T , CustomGene, PartialCustomGeneData, CompleteCustomGeneData, ValidGene] {

    override def newInstance[NT <: GeneStatus](id: Option[String], name: Option[String], properties: Map[String, Class[_]],
                                               alleles: Iterable[AlleleBuilder[_]])(implicit tt: TypeTag[NT]): CustomGeneBuilder[NT] = {
      new CustomGeneBuilderImpl[NT](id, name, properties, alleles, _conversionMap)
    }

    override def setCustomProperties(properties: Map[String, PropertyInfo]): CustomGeneBuilder[T with GeneWithProperties with GeneWithConversionMap] = {
      new CustomGeneBuilderImpl(_id, _name, properties.keySet.map((_, classOf[Double])).toMap, _alleles,
        properties.map({case (k,v) => (k, v.conversionMap)}))
    }

    override def setConversionMap(conversionMap: Map[String, Map[String, Double]]): CustomGeneBuilder[T with GeneWithConversionMap] =
      new CustomGeneBuilderImpl(_id, _name, _properties, _alleles, conversionMap)

    override def tryCompleteBuild(): Try[CompleteCustomGeneData] = {
      status.tpe match {
        case t if t <:< typeOf[CustomGene] =>
          val check = completeGeneRequirements()
          val exception = checkProperties ++: check._1
          if(exception.isEmpty) {
            Success(new CustomGeneDataImpl(_id, _name.get, _properties, check._2, _conversionMap) with CompleteCustomGeneData)
          } else {
            Failure(exception.get)
          }
        case _ =>
          Failure(checkProperties().get)
      }
    }

    override def checkProperties(): Option[CompleteBuildException] = {
      var exception = super.checkProperties()
      if(!_conversionMap.isValid())
        exception = exception ++: InvalidParamValueBuildException("Gene: " + _name.get, "conversion map", _conversionMap)
      exception
    }

    override protected def buildPartialInstance(): PartialCustomGeneData =
      new CustomGeneDataImpl(_id, _name.get, _properties, _alleles.map(_.build()), _conversionMap)
  }

  private[this] class CustomGeneDataImpl[A <: PartialAlleleData](_id: Option[String],
                                                           _name: String,
                                                           _properties: Map[String, Class[_]],
                                                           _alleleData: Iterable[A],
                                                           _conversionMap: Map[String, Map[String, Double]])
    extends DefaultGeneDataImpl(_id, _name, _properties, _alleleData) with CustomGeneData[A] {

    import BuildersValidationImplicits._

    override val getConversionMap: Option[Map[String, Map[String, Double]]] = _conversionMap.boxToValidOption()
  }
}
