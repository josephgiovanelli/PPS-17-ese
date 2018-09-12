package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.beans.Gene
import it.unibo.pps.ese.controller.loader.data.CustomGeneData.CustomGeneDataImpl
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.DefaultGeneDataImpl
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder.GeneStatus
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder.GeneStatus._

import scala.reflect.runtime.universe._

trait GeneBuilder[T <: GeneStatus] {
  def setId(id: String): GeneBuilder[T with GeneWithId]
  def setName(name: String): GeneBuilder[T with GeneWithName]
  def addProperties(properties: Map[String, Class[_]]): GeneBuilder[T with GeneWithProperties]
  def addAlleles(alleles: Set[AlleleData]): GeneBuilder[T with GeneWithAlleles]
  def addConversionMap(conversionMap: Map[String, Map[String, Double]]): GeneBuilder[T with GeneWithConversionMap]
  def buildDefault(): PartialDefaultGeneData
  def buildCompleteDefault(implicit ev: T =:= DefaultGene, st: TypeTag[T]): CompleteDefaultGeneData
  def buildCustom(): PartialCustomGeneData
  def buildCompleteCustom(implicit ev: T =:= CustomGene, st: TypeTag[T]): CompleteCustomGeneData
  def status: TypeTag[T]
}

object GeneBuilder {

  def apply(): GeneBuilder[EmptyGene] = new GeneBuilderImpl[EmptyGene](null, null, Map(), Set(), Map())

  private class GeneBuilderImpl[T <: GeneStatus](id: String,
                                                 name: String,
                                                 properties: Map[String, Class[_]],
                                                 alleles: Set[AlleleData],
                                                 conversionMap: Map[String, Map[String, Double]])
                                                (implicit val status: TypeTag[T]) extends GeneBuilder[T] {

    def setId(id: String): GeneBuilder[T with GeneWithId] =
      new GeneBuilderImpl(id, name, properties, alleles, conversionMap)

    def setName(name: String): GeneBuilder[T with GeneWithName] =
      new GeneBuilderImpl(id, name, properties, alleles, conversionMap)

    def addProperties(properties: Map[String, Class[_]]): GeneBuilder[T with GeneWithProperties] =
      new GeneBuilderImpl(id, name, properties, alleles, conversionMap)

    def addAlleles(alleles: Set[AlleleData]): GeneBuilder[T with GeneWithAlleles] = {
      require(alleles.nonEmpty)
      new GeneBuilderImpl(id, name, properties, alleles, conversionMap)
    }

    def addConversionMap(conversionMap: Map[String, Map[String, Double]]): GeneBuilder[T with GeneWithConversionMap] =
      new GeneBuilderImpl(id, name, properties, alleles, conversionMap)

    def setDefaultInfo(defaultGene: it.unibo.pps.ese.controller.loader.DefaultGene): GeneBuilder[T with DefaultGeneTemplate] =
      new GeneBuilderImpl(id, defaultGene.name, defaultGene.properties, alleles, conversionMap)

    def setCustomInfo(gene: Gene): GeneBuilder[T with CustomGeneTemplate with GeneWithId] =
      new GeneBuilderImpl(gene.id, gene.simpleName, gene.properties.keySet.map((_, classOf[Double])).toMap, alleles,
        gene.properties.map({case (k,v) => (k, v.conversionMap)}))

    def buildDefault(): PartialDefaultGeneData = {
      //require(status.tpe <:< st.tpe)
      //TODO check no conversion map
      status.tpe match {
        case t if t <:< typeOf[DefaultGene] =>
          val illegalState = completeGeneRequirements
          if(illegalState.isEmpty) {
            new DefaultGeneDataImpl(id, name, properties, alleles) with CompleteDefaultGeneData
          } else {
            new DefaultGeneDataImpl(id, name, properties, alleles) with PartialDefaultGeneData
          }
        case _ =>
          new DefaultGeneDataImpl(id, name, properties, alleles) with PartialDefaultGeneData
      }
    }

    def buildCompleteDefault(implicit ev: T =:= DefaultGene, st: TypeTag[T]): CompleteDefaultGeneData = {
      //TODO in all builders
      require(status.tpe <:< st.tpe)
      val illegalState = completeGeneRequirements
      if(illegalState.isDefined)
        throw illegalState.get
      new DefaultGeneDataImpl(id, name, properties, alleles) with CompleteDefaultGeneData
    }

    def buildCustom(): PartialCustomGeneData = {
      //require(status.tpe <:< st.tpe)
      //TODO resolve ambiguity with Default
      status.tpe match {
        case t if t <:< typeOf[CustomGene] =>
          val illegalState = completeGeneRequirements
          if(illegalState.isEmpty) {
            new CustomGeneDataImpl(id, name, properties, alleles, conversionMap) with CompleteCustomGeneData
          } else {
            new CustomGeneDataImpl(id, name, properties, alleles, conversionMap) with PartialCustomGeneData
          }
        case _ =>
          new CustomGeneDataImpl(id, name, properties, alleles, conversionMap) with PartialCustomGeneData
      }
    }

    def buildCompleteCustom(implicit ev: T =:= CustomGene, st: TypeTag[T]): CompleteCustomGeneData = {
      require(status.tpe <:< st.tpe)
      val illegalState = completeGeneRequirements
      if(illegalState.isDefined)
        throw illegalState.get
      new CustomGeneDataImpl(id, name, properties, alleles, conversionMap) with CompleteCustomGeneData
    }

    private def completeGeneRequirements: Option[Exception] = {
      if(alleles.map(_.probability).sum == 1.0 &&
        alleles.forall(_.effect.keySet.subsetOf(properties.keySet)) &&
        alleles.forall(_.gene == id)) {
        None
      } else {
        Some(new IllegalStateException())
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
}