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

trait CustomGeneBuilder[S <: GeneStatus] extends GeneBuilder[S] with GenericBuilder[S, CustomGene, PartialCustomGeneData, CompleteCustomGeneData] {
  type RET[A <: S] = CustomGeneBuilder[A]
  def setCustomProperties(properties: Map[String, PropertyInfo]): CustomGeneBuilder[S with GeneWithProperties with GeneWithConversionMap]
  def addConversionMap(conversionMap: Map[String, Map[String, Double]]): CustomGeneBuilder[S with GeneWithConversionMap]
}

object CustomGeneBuilder {

  def apply(): CustomGeneBuilder[EmptyGene] = new CustomGeneBuilderImpl[EmptyGene](None, None, Map(), Iterable(), Map())

  private[this] class CustomGeneBuilderImpl[T <: GeneStatus](id: Option[String],
                                               name: Option[String],
                                               properties: Map[String, Class[_]],
                                               alleles: Iterable[AlleleBuilder[_]],
                                               conversionMap: Map[String, Map[String, Double]])
                                              (implicit val status: TypeTag[T])
    extends GenericGeneBuilderImpl[T](id, name, properties, alleles) with CustomGeneBuilder[T]
      with BaseBuildableGenericBuilder[T , CustomGene, PartialCustomGeneData, CompleteCustomGeneData]
      with ValidStatusGenericBuilder[T , CustomGene, PartialCustomGeneData, CompleteCustomGeneData, ValidGene] {

    override def newInstance[NT <: GeneStatus](id: Option[String], name: Option[String], properties: Map[String, Class[_]],
                                               alleles: Iterable[AlleleBuilder[_]])(implicit tt: TypeTag[NT]): CustomGeneBuilder[NT] = {
      new CustomGeneBuilderImpl[NT](id, name, properties, alleles, conversionMap)
    }

    override def setCustomProperties(properties: Map[String, PropertyInfo]): CustomGeneBuilder[T with GeneWithProperties with GeneWithConversionMap] = {
      new CustomGeneBuilderImpl(id, name, properties.keySet.map((_, classOf[Double])).toMap, alleles,
        properties.map({case (k,v) => (k, v.conversionMap)}))
    }

    override def addConversionMap(conversionMap: Map[String, Map[String, Double]]): CustomGeneBuilder[T with GeneWithConversionMap] =
      new CustomGeneBuilderImpl(id, name, properties, alleles, conversionMap)

    override def tryCompleteBuild(): Try[CompleteCustomGeneData] = {
      status.tpe match {
        case t if t <:< typeOf[CustomGene] =>
          val check = completeGeneRequirements
          val exception = checkProperties ++: check._1
          if(exception.isEmpty) {
            Success(new CustomGeneDataImpl(id, name.get, properties, check._2, conversionMap) with CompleteCustomGeneData)
          } else {
            Failure(exception.get)
          }
        case _ =>
          Failure(checkProperties.get)
      }
    }

    override def checkProperties: Option[CompleteBuildException] = {
      var exception = super.checkProperties
      if(!conversionMap.isValid())
        exception = exception ++: InvalidParamValueBuildException("Gene: " + name.get, "conversion map", conversionMap)
      exception
    }

    override protected def buildPartialInstance(): PartialCustomGeneData =
      new CustomGeneDataImpl(id, name.get, properties, alleles.map(_.build()), conversionMap)
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
