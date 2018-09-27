package it.unibo.pps.ese.controller.simulation.loader.data.builder.gene

import it.unibo.pps.ese.controller.simulation.loader.data.GeneData.{CompleteGeneData, PartialGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.GeneStatus._

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._

/** Simple trait that represent a generic gene's builder without build methods. Defines fields' setters common between
  * [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.DefaultGeneBuilder]] and
  * [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.CustomGeneBuilder]]
  *
  * @tparam S Current builder's status
  */
private[builder] trait GenericGeneBuilder[S <: GeneStatus] extends BuilderContent {
  /** Abstract type represent concrete builder type (extension of GenericGeneBuilder)*/
  type RET[A <: S] <: GenericGeneBuilder[A]
  /** Set gene's id
    *
    * @param id Gene's id
    * @return New builder with updated param and status
    */
  def setId(id: String): RET[S with GeneWithId]
  /** Set gene's name
    *
    * @param name Gene's name
    * @return New builder with updated param and status
    */
  def setName(name: String): RET[S with GeneWithName]
  //TODO set properties
  /** Add gene's properties represented by a map containing qualities name as key and their type as value
    *
    * @param properties Gene's id
    * @return New builder with updated param and status
    */
  def addProperties(properties: Map[String, Class[_]]): RET[S with GeneWithProperties]
  /** Add gene's alleles as list of builders
    *
    * @param alleles Gene's alleles builders
    * @return New builder with updated param and status
    */
  def addAlleles(alleles: Iterable[AlleleBuilder[_]]): RET[S with GeneWithAlleles]
}

/** Trait that adds to [[it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.GenericGeneBuilder]] dynamic
  * build methods. Build methods can create generic data types [[it.unibo.pps.ese.controller.simulation.loader.data.GeneData.PartialGeneData]]
  * and [[it.unibo.pps.ese.controller.simulation.loader.data.GeneData.PartialGeneData]]
  *
  * @tparam S Current builder's status
  */
private[builder] trait GeneBuilder[S <: GeneStatus] extends GenericGeneBuilder[S]
  with DynamicBuilder[PartialGeneData, CompleteGeneData]

private[gene] abstract class GenericGeneBuilderImpl[T <: GeneStatus: TypeTag](_id: Option[String],
                                                                              _name: Option[String],
                                                                              _properties: Map[String, Class[_]],
                                                                              _alleles: Iterable[AlleleBuilder[_]])
                                               (implicit val validStatus: TypeTag[ValidGene])
                                               extends GenericGeneBuilder[T] { self =>

  def setId(id: String): RET[T with GeneWithId] =
    newInstance(Some(id), _name, _properties, _alleles)

  def setName(name: String): RET[T with GeneWithName] =
    newInstance(_id, Some(name), _properties, _alleles)

  def addProperties(properties: Map[String, Class[_]]): RET[T with GeneWithProperties] =
    newInstance(_id, _name, properties, _alleles)

  override def addAlleles(alleles: Iterable[AlleleBuilder[_]]): RET[T with GeneWithAlleles] =
    newInstance(_id, _name, _properties, _alleles ++ alleles)

  /** template method that creates new instance of builder type, declared abstract in this class
    *
    * @param id New gene's builder id
    * @param name New gene's builder name
    * @param properties New gene's builder properties
    * @param alleles New gene's builder id alleles
    * @tparam NT New builder's status
    * @return New builder instance
    */
  def newInstance[NT <: T](id: Option[String], name: Option[String], properties: Map[String, Class[_]],
                           alleles: Iterable[AlleleBuilder[_]])(implicit tt: TypeTag[NT]): RET[NT]

  /** Check builder status depending by internal alleles' builders
    *
    * @return Optional exception occurred during checks and collection of complete alleles builded
    */
  protected def completeGeneRequirements(): (Option[CompleteBuildException], Iterable[CompleteAlleleData]) = {
    var exception: Option[CompleteBuildException] = None
    val tries: Iterable[Try[CompleteAlleleData]] = _alleles.map(_.tryCompleteBuild())
    val all: Iterable[CompleteAlleleData] = tries.collect({case Success(value) => value})
    if(all.size != _alleles.size) {
      exception = exception ++: CompleteBuildException("Gene: " + _name.get + " | all alleles must be complete",
        tries.collect({case Failure(value: CompleteBuildException) => value}))
    }
    if(!(all.map(_.probability).sum == 1.0))
      exception = exception ++: CompleteBuildException("Gene: " + _name.get + " | sum of alleles' probabilities must be 1")
    if(!all.forall(_.effect.keySet.subsetOf(_properties.keySet)))
      exception = exception ++: CompleteBuildException("Gene: " + _name.get + " | alleles must effect only gene's properties")
    if(!all.forall(a => _id.contains(a.gene)))
      exception = exception ++: CompleteBuildException("Gene: " + _name.get + " | alleles must refer to gene's id")
    (exception, all)
  }

  /** Method checks mandatory fields required for a partial data build
    *
    * @return Exception representing mandatory fields missing or invalidity
    */
  protected def checkMandatoryProperties(): Option[CompleteBuildException] = {
    if(!_name.isValid())
      Some(InvalidParamValueBuildException("Gene: " + _name.get, "name", _name))
    else
      None
  }

  /** Check builder status depending by fields value
    *
    * @return Optional exception occurred during checks
    */
  protected def checkProperties(): Option[CompleteBuildException] = {
    var exception: Option[CompleteBuildException] = checkMandatoryProperties()
    if(!_id.isValid())
      exception = exception ++: InvalidParamValueBuildException("Gene: " + _name.get, "id", _id)
    if(!_properties.isValid())
      exception = exception ++: InvalidParamValueBuildException("Gene: " + _name.get, "properties", _properties)
    if(!_alleles.isValid())
      exception = exception ++: InvalidParamValueBuildException("Gene: " + _name.get, "alleles", _alleles)
    exception
  }
}

/** Interface that represent generic gene's builder's status*/
sealed trait GeneStatus extends BuilderStatus
/** Object containing all possible gene's builder's statuses*/
object GeneStatus {
  sealed trait EmptyGene extends GeneStatus
  sealed trait GeneWithId extends GeneStatus
  sealed trait GeneWithName extends GeneStatus
  sealed trait GeneWithProperties extends GeneStatus
  sealed trait GeneWithAlleles extends GeneStatus
  sealed trait GeneWithConversionMap extends GeneStatus

  /**Type that defines valid gene's builder's status*/
  type ValidGene = EmptyGene with GeneWithName
  type DefaultGeneTemplate =  ValidGene with GeneWithProperties
  type CustomGeneTemplate = DefaultGeneTemplate with GeneWithConversionMap
  /**Type that defines complete default gene's builder's status*/
  type DefaultGene = DefaultGeneTemplate with GeneWithId with GeneWithAlleles
  /**Type that defines complete custom gene's builder's status*/
  type CustomGene = CustomGeneTemplate with GeneWithId with GeneWithAlleles
}