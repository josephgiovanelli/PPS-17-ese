package it.unibo.pps.ese.controller.simulation.loader.data.builder.gene

import it.unibo.pps.ese.controller.simulation.loader.data.GeneData.{CompleteGeneData, PartialGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.GeneStatus._

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._

trait GenericGeneBuilder[S <: GeneStatus] extends BuilderContent[S] { self =>
  type RET[A <: S] <: GenericGeneBuilder[A]
  def setId(id: String): RET[S with GeneWithId]
  def setName(name: String): RET[S with GeneWithName]
  def addProperties(properties: Map[String, Class[_]]): RET[S with GeneWithProperties]
  def addAlleles(alleles: Iterable[AlleleBuilder[_]]): RET[S with GeneWithAlleles]
  def status: TypeTag[S]
}

trait GeneBuilder[S <: GeneStatus] extends GenericGeneBuilder[S] with StaticBuilder[S, PartialGeneData, CompleteGeneData]

private[gene] abstract class GenericGeneBuilderImpl[T <: GeneStatus](id: Option[String],
                                               name: Option[String],
                                               properties: Map[String, Class[_]],
                                               alleles: Iterable[AlleleBuilder[_]])
                                               (implicit private val test: TypeTag[T], val validStatus: TypeTag[ValidGene])
                                               extends GenericGeneBuilder[T] { self =>

  def setId(id: String): RET[T with GeneWithId] =
    newInstance(Some(id), name, properties, alleles)

  def setName(name: String): RET[T with GeneWithName] =
    newInstance(id, Some(name), properties, alleles)

  def addProperties(properties: Map[String, Class[_]]): RET[T with GeneWithProperties] =
    newInstance(id, name, properties, alleles)

  override def addAlleles(alleles: Iterable[AlleleBuilder[_]]): RET[T with GeneWithAlleles] =
    newInstance(id, name, properties, alleles)

  def newInstance[NT <: T](id: Option[String], name: Option[String], properties: Map[String, Class[_]],
                           alleles: Iterable[AlleleBuilder[_]])(implicit tt: TypeTag[NT]): RET[NT]

  protected def completeGeneRequirements: (Option[CompleteBuildException], Iterable[CompleteAlleleData]) = {
    var exception: Option[CompleteBuildException] = None
    val tries: Iterable[Try[CompleteAlleleData]] = alleles.map(_.tryCompleteBuild())
    val all: Iterable[CompleteAlleleData] = tries.collect({case Success(value) => value})
    if(all.size != alleles.size) {
      exception = exception ++: CompleteBuildException("Gene: " + name.get + " | all alleles must be complete",
        tries.collect({case Failure(value: CompleteBuildException) => value}))
    }
    if(!(all.map(_.probability).sum == 1.0))
      exception = exception ++: CompleteBuildException("Gene: " + name.get + " | sum of alleles' probabilities must be 1")
    if(!all.forall(_.effect.keySet.subsetOf(properties.keySet)))
      exception = exception ++: CompleteBuildException("Gene: " + name.get + " | alleles must effect only gene's properties")
    if(!all.forall(a => id.contains(a.gene)))
      exception = exception ++: CompleteBuildException("Gene: " + name.get + " | alleles must refer to gene's id")
    (exception, all)
  }

  protected def checkMandatoryProperties: Option[CompleteBuildException] = {
    if(!name.isValid())
      Some(InvalidParamValueBuildException("Gene: " + name.get, "name", name))
    else
      None
  }

  protected def checkProperties: Option[CompleteBuildException] = {
    var exception: Option[CompleteBuildException] = checkMandatoryProperties
    if(!id.isValid())
      exception = exception ++: InvalidParamValueBuildException("Gene: " + name.get, "id", id)
    if(!properties.isValid())
      exception = exception ++: InvalidParamValueBuildException("Gene: " + name.get, "properties", properties)
    if(!alleles.isValid())
      exception = exception ++: InvalidParamValueBuildException("Gene: " + name.get, "alleles", alleles)
    exception
  }
}

sealed trait GeneStatus extends BuilderStatus
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