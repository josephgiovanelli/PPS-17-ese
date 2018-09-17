package it.unibo.pps.ese.controller.loader.data.builder.gene

import it.unibo.pps.ese.controller.loader.beans.PropertyInfo
import it.unibo.pps.ese.controller.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.loader.data.GeneData.PartialGeneData
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.AlleleBuilder
import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.loader.data.builder.gene.GeneStatus._

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

trait GenericGeneBuilder[S <: GeneStatus] { self =>
  type RET[A <: GeneStatus]
  def setId(id: String): RET[S with GeneWithId]
  def setName(name: String): RET[S with GeneWithName]
  def addProperties(properties: Map[String, Class[_]]): RET[S with GeneWithProperties]
  def addAlleles(alleles: Iterable[AlleleBuilder[_]]): RET[S with GeneWithAlleles]
  def status: TypeTag[S]
}

trait BuildableGeneBuilder[S <: GeneStatus, CS <: GeneStatus, P <: PartialGeneData, C <: P] extends GenericGeneBuilder[S]{
  def build(): P
  def tryCompleteBuild(): Try[C]
  def buildComplete(implicit ev: S =:= CS, st: TypeTag[S]): C
}

abstract class GenericGeneBuilderImpl[T <: GeneStatus](id: Option[String],
                                               name: Option[String],
                                               properties: Map[String, Class[_]],
                                               alleles: Iterable[AlleleBuilder[_]])
                                                      (implicit private val test: TypeTag[T])
                                               extends GenericGeneBuilder[T] { self =>

  def setId(id: String): RET[T with GeneWithId] =
    newInstance[T with GeneWithId](Some(id), name, properties, alleles)

  def setName(name: String): RET[T with GeneWithName] =
    newInstance[T with GeneWithName](id, Some(name), properties, alleles)

  def addProperties(properties: Map[String, Class[_]]): RET[T with GeneWithProperties] =
    newInstance[T with GeneWithProperties](id, name, properties, alleles)

  override def addAlleles(alleles: Iterable[AlleleBuilder[_]]): RET[T with GeneWithAlleles] =
    newInstance[T with GeneWithAlleles](id, name, properties, alleles)

  def newInstance[NT <: GeneStatus](id: Option[String], name: Option[String], properties: Map[String, Class[_]],
                           alleles: Iterable[AlleleBuilder[_]])(implicit tt: TypeTag[NT]): RET[NT]

  protected def completeGeneRequirements: (Option[Exception], Iterable[CompleteAlleleData]) = {
    var exception: Option[CompleteBuildException] = None
    val tries: Iterable[Try[CompleteAlleleData]] = alleles.map(_.tryCompleteBuild())
    val all: Iterable[CompleteAlleleData] = tries.collect({case Success(value) => value})
    if(all.size != alleles.size) {
      exception = exception ++: new CompleteBuildException("Gene: " + name.get + " | all alleles must be complete",
        tries.collect({case Failure(value: CompleteBuildException) => value}))
    }
    if(!(all.map(_.probability).sum == 1.0))
      exception = exception ++: new CompleteBuildException("Gene: " + name.get + " | sum of alleles' probabilities must be 1")
    if(!all.forall(_.effect.keySet.subsetOf(properties.keySet)))
      exception = exception ++: new CompleteBuildException("Gene: " + name.get + " | alleles problem")
    if(!all.forall(a => id.contains(a.gene)))
      exception = exception ++: new CompleteBuildException("Gene: " + name.get + " | alleles problem")
    (exception, all)
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

  type ValidGene = EmptyGene with GeneWithName
  type DefaultGeneTemplate =  ValidGene with GeneWithProperties
  type CustomGeneTemplate = DefaultGeneTemplate with GeneWithConversionMap
  type DefaultGene = DefaultGeneTemplate with GeneWithId with GeneWithAlleles
  type CustomGene = CustomGeneTemplate with GeneWithId with GeneWithAlleles
}