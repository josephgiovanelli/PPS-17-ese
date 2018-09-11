package it.unibo.pps.ese.controller.loader.data.builder

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
  def buildDefault(implicit st: TypeTag[T]): DefaultGeneData
  def buildCompleteDefault(implicit ev: T =:= DefaultGene, st: TypeTag[T]): CompleteDefaultGeneData
  def buildCustom(implicit st: TypeTag[T]): CustomGeneData
  def buildCompleteCustom(implicit ev: T =:= CustomGene, st: TypeTag[T]): CompleteCustomGeneData
  def status: TypeTag[T]
}

object GeneBuilder {

  def apply(): GeneBuilder[EmptyGene] = new GeneBuilderImpl[EmptyGene]()

  private class GeneBuilderImpl[T <: GeneStatus](id: String = null,
                                     name: String = null,
                                     properties: Map[String, Class[_]] = Map(),
                                     alleles: Set[AlleleData] = Set(),
                                     conversionMap: Map[String, Map[String, Double]] = Map())
                                                (implicit val status: TypeTag[T])extends GeneBuilder[T] {

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

    def buildDefault(implicit st: TypeTag[T]): DefaultGeneData = {
      require(status.tpe <:< st.tpe)
      //TODO check no conversion map
      st.tpe match {
        case t if t <:< typeOf[DefaultGene] =>
          val illegalState = completeGeneRequirements
          if(illegalState.isEmpty) {
            new DefaultGeneDataImpl(name, id, properties, alleles) with CompleteDefaultGeneData
          } else {
            new DefaultGeneDataImpl(name, id, properties, alleles) with PartialDefaultGeneData
          }
        case _ =>
          new DefaultGeneDataImpl(name, id, properties, alleles) with PartialDefaultGeneData
      }
    }

    def buildCompleteDefault(implicit ev: T =:= DefaultGene, st: TypeTag[T]): CompleteDefaultGeneData = {
      //TODO in all builders
      require(status.tpe <:< st.tpe)
      val illegalState = completeGeneRequirements
      if(illegalState.isDefined)
        throw illegalState.get
      new DefaultGeneDataImpl(name, id, properties, alleles) with CompleteDefaultGeneData
    }

    def buildCustom(implicit st: TypeTag[T]): CustomGeneData = {
      require(status.tpe <:< st.tpe)
      //TODO check no conversion map
      st.tpe match {
        case t if t =:= typeOf[CustomGene] =>
          val illegalState = completeGeneRequirements
          if(illegalState.isEmpty) {
            new CustomGeneDataImpl(name, id, properties, alleles, conversionMap) with CompleteCustomGeneData
          } else {
            new CustomGeneDataImpl(name, id, properties, alleles, conversionMap) with PartialCustomGeneData
          }
        case _ =>
          new CustomGeneDataImpl(name, id, properties, alleles, conversionMap) with PartialCustomGeneData
      }
    }

    def buildCompleteCustom(implicit ev: T =:= CustomGene, st: TypeTag[T]): CompleteCustomGeneData = {
      require(status.tpe <:< st.tpe)
      val illegalState = completeGeneRequirements
      if(illegalState.isDefined)
        throw illegalState.get
      new CustomGeneDataImpl(name, id, properties, alleles, conversionMap) with CompleteCustomGeneData
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

    type DefaultGeneTemplate = EmptyGene with GeneWithId with GeneWithName with GeneWithProperties
    type CustomGeneTemplate = DefaultGeneTemplate with GeneWithConversionMap
    type DefaultGene = DefaultGeneTemplate with GeneWithAlleles
    type CustomGene = CustomGeneTemplate with GeneWithAlleles
  }
}