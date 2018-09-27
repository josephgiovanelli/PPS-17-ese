package it.unibo.pps.ese.controller.simulation.loader.data.builder.entities

import it.unibo.pps.ese.controller.simulation.loader.Reigns
import it.unibo.pps.ese.controller.simulation.loader.data.EntityData
import it.unibo.pps.ese.controller.simulation.loader.data.EntityData.{CompleteEntityData, PartialEntityData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.{BuilderContent, BuilderStatus, DynamicBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.EntityStatus.{EntityWithAlleleLength, EntityWithGeneLength, EntityWithReign, PlantWithHardness, PlantWithNutritionalValue, _}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}

import scala.reflect.runtime.universe._
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._
import it.unibo.pps.ese.utils.DefaultValidable.ValidableInsideRange._

/** Simple trait that represent a generic entity's builder without build methods. Defines fields' setters common between
  * [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.AnimalBuilder]] and
  * [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.PlantBuilder]]
  *
  * @tparam S Current builder's status
  */
private[builder] trait GenericEntityBuilder[S <: EntityStatus] extends BuilderContent {
  /** Abstract type represent concrete builder type (extension of GenericEntityBuilder)*/
  type RET[A <: S] <: GenericEntityBuilder[A]
  /** Set entity's name
    *
    * @param name Entity's name
    * @return New builder with updated param and status
    */
  def setName(name: String): RET[S with EntityWithName]
  /** Set entity's genes' ids length
    *
    * @param geneLength Entity's genes' ids length
    * @return New builder with updated param and status
    */
  def setGeneLength(geneLength: Int): RET[S with EntityWithGeneLength]
  /** Set entity's alleles' ids length
    *
    * @param alleleLength Entity's alleles' ids length
    * @return New builder with updated param and status
    */
  def setAlleleLength(alleleLength: Int): RET[S with EntityWithAlleleLength]
  /** Set entity's name
    *
    * @param name Entity's name
    * @return New builder with updated param and status
    */
  def setReign(reign: String): RET[S with EntityWithReign]
}

/** Trait that adds to [[it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.GenericEntityBuilder]] dynamic
  * build methods. Build methods can create generic data types [[it.unibo.pps.ese.controller.simulation.loader.data.EntityData.PartialEntityData]]
  * and [[it.unibo.pps.ese.controller.simulation.loader.data.EntityData.CompleteEntityData]]
  *
  * @tparam S Current builder's status
  */
private[builder] trait EntityBuilder[S <: EntityStatus] extends GenericEntityBuilder[S] with DynamicBuilder[PartialEntityData, CompleteEntityData]

private[entities] abstract class EntityBuilderImpl[S <: EntityStatus: TypeTag](name: Option[String],
                                                   geneLength: Option[Int],
                                                   alleleLength: Option[Int],
                                                   reign: Option[String])(implicit val validStatus: TypeTag[ValidEntity]) extends GenericEntityBuilder[S]{

  def setName(name: String): RET[S with EntityWithName] =
    newInstance(Some(name), geneLength, alleleLength, reign)

  def setGeneLength(geneLength: Int): RET[S with EntityWithGeneLength] =
    newInstance(name, Some(geneLength), alleleLength, reign)

  def setAlleleLength(alleleLength: Int): RET[S with EntityWithAlleleLength] =
    newInstance(name, geneLength, Some(alleleLength), reign)

  def setReign(reign: String): RET[S with EntityWithReign] =
    newInstance(name, geneLength, alleleLength, Some(reign))

  /** template method that creates new instance of builder type, declared abstract in this class
    *
    * @param name New entity's builder name
    * @param geneLength New entity's builder geneLength
    * @param alleleLength New entity's builder alleleLength
    * @param reign New entity's builder reign
    * @tparam NT New builder's status
    * @return New builder instance
    */
  def newInstance[NT <: S](name: Option[String], geneLength: Option[Int], alleleLength: Option[Int],
                                      reign: Option[String])(implicit tt: TypeTag[NT]): RET[NT]

  /** Method checks mandatory fields required for a partial data build
    *
    * @return Exception representing mandatory fields missing or invalidity
    */
  protected def checkMandatoryProperties: Option[CompleteBuildException] = {
    if(!name.isValid())
      Some(InvalidParamValueBuildException("Entity " + name.getOrElse(""), "name", name))
    else
      None
  }

  /** Check builder status depending by fields value
    *
    * @return Optional exception occurred during checks
    */
  protected def checkProperties: Option[CompleteBuildException] = {
    var exception: Option[CompleteBuildException] = checkMandatoryProperties
    if(!geneLength.inValidRange())
      exception = exception ++: InvalidParamValueBuildException("Entity " + name.getOrElse(""), "geneLength", geneLength)
    if(!alleleLength.inValidRange())
      exception = exception ++: InvalidParamValueBuildException("Entity " + name.getOrElse(""), "alleleLength", alleleLength)
    if(!reign.isValid(r => Reigns.elements.map(_.code).contains(r)))
      exception = exception ++: InvalidParamValueBuildException("Entity " + name.getOrElse(""), "reign", reign)
    exception
  }
}

/** Interface that represent generic entity's builder's status*/
sealed trait EntityStatus extends BuilderStatus
/** Object containing all possible entity's builder's statuses*/
object EntityStatus {
  sealed trait EmptyEntity extends EntityStatus
  sealed trait EntityWithName extends EntityStatus
  sealed trait EntityWithGeneLength extends EntityStatus
  sealed trait EntityWithAlleleLength extends EntityStatus
  sealed trait EntityWithReign extends EntityStatus
  sealed trait PlantWithNutritionalValue extends EntityStatus
  sealed trait PlantWithHardness extends EntityStatus
  sealed trait PlantWithHeight extends EntityStatus
  sealed trait AnimalWithTypology extends EntityStatus
  sealed trait AnimalWithStructChromosome extends EntityStatus
  sealed trait AnimalWithRegChromosome extends EntityStatus
  sealed trait AnimalWithSexChromosome extends EntityStatus

  /**Type that defines valid entity's builder's status*/
  type ValidEntity = EmptyEntity with EntityWithName
  type FullEntity = ValidEntity with  EntityWithGeneLength with EntityWithAlleleLength with EntityWithReign
  /**Type that defines complete plant's builder's status*/
  type FullPlant = FullEntity  with PlantWithHeight with PlantWithNutritionalValue with PlantWithHardness
  /**Type that defines complete plant's builder's status*/
  type FullAnimal = FullEntity with AnimalWithTypology with AnimalWithStructChromosome with AnimalWithRegChromosome
    with AnimalWithSexChromosome
}

private class EntityDataImpl(val name: String,
                             _getGeneLength: Option[Int],
                             _getAlleleLength: Option[Int],
                             _getReign: Option[String]) extends EntityData {
  import it.unibo.pps.ese.controller.simulation.loader.data.builder.BuildersValidationImplicits._

  override val getGeneLength: Option[Int] = _getGeneLength.normalize()
  override val getAlleleLength: Option[Int] = _getAlleleLength.normalize()
  override val getReign: Option[String] = _getReign.normalize()
}
