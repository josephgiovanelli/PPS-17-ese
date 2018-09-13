package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.beans.Gene
import it.unibo.pps.ese.controller.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder.GeneStatus
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder.GeneStatus._

import scala.reflect.runtime.universe._

trait GeneBuilder[T <: GeneStatus] {
  def setId(id: String): GeneBuilder[T with GeneWithId]
  def setName(name: String): GeneBuilder[T with GeneWithName]
  def addProperties(properties: Map[String, Class[_]]): GeneBuilder[T with GeneWithProperties]
  def addAlleles(alleles: Iterable[CompleteAlleleData]): GeneBuilder[T with GeneWithAlleles]
  def addConversionMap(conversionMap: Map[String, Map[String, Double]]): GeneBuilder[T with GeneWithConversionMap]
  def buildDefault(): PartialDefaultGeneData
  def buildCompleteDefault(implicit ev: T =:= DefaultGene, st: TypeTag[T]): CompleteDefaultGeneData
  def buildCustom(): PartialCustomGeneData
  def buildCompleteCustom(implicit ev: T =:= CustomGene, st: TypeTag[T]): CompleteCustomGeneData
  def status: TypeTag[T]
  def setDefaultInfo(defaultGene: it.unibo.pps.ese.controller.loader.DefaultGene): GeneBuilder[T with DefaultGeneTemplate]
  def setCustomInfo(gene: Gene): GeneBuilder[T with CustomGeneTemplate with GeneWithId]
}

object GeneBuilder {

  def apply(): GeneBuilder[EmptyGene] = new GeneBuilderImpl[EmptyGene](null, null, Map(), Set(), Map())

  private class GeneBuilderImpl[T <: GeneStatus](id: Option[String],
                                                 name: Option[String],
                                                 properties: Map[String, Class[_]],
                                                 alleles: Iterable[_ <: PartialAlleleData],
                                                 conversionMap: Map[String, Map[String, Double]])
                                                (implicit val status: TypeTag[T]) extends GeneBuilder[T] {

    def setId(id: String): GeneBuilder[T with GeneWithId] =
      new GeneBuilderImpl(Some(id), name, properties, alleles, conversionMap)

    def setName(name: String): GeneBuilder[T with GeneWithName] =
      new GeneBuilderImpl(id, Some(name), properties, alleles, conversionMap)

    def addProperties(properties: Map[String, Class[_]]): GeneBuilder[T with GeneWithProperties] =
      new GeneBuilderImpl(id, name, properties, alleles, conversionMap)

    def addAlleles(alleles: Iterable[CompleteAlleleData]): GeneBuilder[T with GeneWithAlleles] = {
      require(alleles.nonEmpty)
      new GeneBuilderImpl(id, name, properties, alleles, conversionMap)
    }

    def addConversionMap(conversionMap: Map[String, Map[String, Double]]): GeneBuilder[T with GeneWithConversionMap] =
      new GeneBuilderImpl(id, name, properties, alleles, conversionMap)

    def setDefaultInfo(defaultGene: it.unibo.pps.ese.controller.loader.DefaultGene): GeneBuilder[T with DefaultGeneTemplate] =
      new GeneBuilderImpl(id, Some(defaultGene.name), defaultGene.properties, alleles, conversionMap)

    def setCustomInfo(gene: Gene): GeneBuilder[T with CustomGeneTemplate with GeneWithId] =
      new GeneBuilderImpl(Some(gene.id), Some(gene.simpleName), gene.properties.keySet.map((_, classOf[Double])).toMap, alleles,
        gene.properties.map({case (k,v) => (k, v.conversionMap)}))

    def buildDefault(): PartialDefaultGeneData = {
      //require(status.tpe <:< st.tpe)
      //TODO check no conversion map
      status.tpe match {
        case t if t <:< typeOf[DefaultGene] =>
          val check = completeGeneRequirements
          if(check._1.isEmpty) {
            new DefaultGeneDataImpl(id, name, properties, check._2) with FullDefaultGeneData[CompleteAlleleData]
          } else {
            new DefaultGeneDataImpl(id, name, properties, alleles)
          }
        case _ =>
          new DefaultGeneDataImpl(id, name, properties, alleles)
      }
    }

    def buildCompleteDefault(implicit ev: T =:= DefaultGene, st: TypeTag[T]): CompleteDefaultGeneData = {
      //TODO in all builders
      require(status.tpe <:< st.tpe)
      val check = completeGeneRequirements
      check._1.foreach(throw _)
      new DefaultGeneDataImpl(id, name, properties, check._2) with FullDefaultGeneData[CompleteAlleleData]
    }

    def buildCustom(): PartialCustomGeneData = {
      //require(status.tpe <:< st.tpe)
      //TODO resolve ambiguity with Default
      status.tpe match {
        case t if t <:< typeOf[CustomGene] =>
          val check = completeGeneRequirements
          if(check._1.isEmpty) {
            new CustomGeneDataImpl(id, name, properties, check._2, conversionMap) with FullCustomGeneData[CompleteAlleleData]
          } else {
            new CustomGeneDataImpl(id, name, properties, alleles, conversionMap)
          }
        case _ =>
          new CustomGeneDataImpl(id, name, properties, alleles, conversionMap)
      }
    }

    def buildCompleteCustom(implicit ev: T =:= CustomGene, st: TypeTag[T]): CompleteCustomGeneData = {
      require(status.tpe <:< st.tpe)
      val check = completeGeneRequirements
      check._1.foreach(throw _)
      new CustomGeneDataImpl(id, name, properties, check._2, conversionMap) with FullCustomGeneData[CompleteAlleleData]
    }

    private def completeGeneRequirements: (Option[Exception], Iterable[CompleteAlleleData]) = {
      var exception: Exception = null
      val all: Iterable[CompleteAlleleData] = alleles.flatMap({
        case c: CompleteAlleleData =>
          Some(c)
        case _ =>
          None
      })
      if(all.size != all.size) {
        exception = new IllegalStateException()
      }
      if(!(all.map(_.probability).sum == 1.0 &&
        all.forall(_.effect.keySet.subsetOf(properties.keySet)) &&
        all.forall(a => id.contains(a.gene)))) {
        exception = new IllegalStateException()
      }
      if(exception == null) {
        (None, all)
      } else {
        (Some(exception), Seq())
      }
    }
  }

  sealed trait GeneStatus
  object GeneStatus {
    sealed trait EmptyGene extends GeneStatus
    sealed trait GeneWithId extends GeneStatus
    sealed trait GeneWithName extends GeneStatus
    sealed trait GeneWithProperties extends GeneStatus
    sealed trait GeneWithAlleles extends GeneStatus
    sealed trait GeneWithConversionMap extends GeneStatus

    type DefaultGeneTemplate = EmptyGene with GeneWithName with GeneWithProperties
    type CustomGeneTemplate = DefaultGeneTemplate with GeneWithConversionMap
    type DefaultGene = DefaultGeneTemplate with GeneWithId with GeneWithAlleles
    type CustomGene = CustomGeneTemplate with GeneWithId with GeneWithAlleles
  }

  private class DefaultGeneDataImpl[A <: PartialAlleleData](override val getId: Option[String],
                                    override val getName: Option[String],
                                    _getProperties: Map[String, Class[_]],
                                    _getAlleles: Iterable[A]) extends DefaultGeneData[A] {
    override val getProperties: Option[Map[String, Class[_]]] = if(_getProperties.isEmpty) None else Some(_getProperties)
    //TODO to set?????
    override val getAlleles: Option[Set[A]] = if(_getAlleles.isEmpty) None else Some(_getAlleles.toSet)
  }

  private class CustomGeneDataImpl[A <: PartialAlleleData](_id: Option[String],
                                   _name: Option[String],
                                   _properties: Map[String, Class[_]],
                                   _alleleData: Iterable[A],
                                   _conversionMap: Map[String, Map[String, Double]])
    extends DefaultGeneDataImpl(_id, _name, _properties, _alleleData) with CustomGeneData[A] {

    override val getConversionMap: Option[Map[String, Map[String, Double]]] = if(_conversionMap.isEmpty) None else Some(_conversionMap)
    //TODO check conversion map with base qualities
  }
}