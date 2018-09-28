package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.utils.{DefaultRange, DefaultValue, InclusiveDefaultRange}

import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._

/** Object contains default implicits used by builders for data validation*/
object BuildersValidationImplicits {
  implicit val int: DefaultRange[Int] = InclusiveDefaultRange(0, 100)
  implicit val double: DefaultRange[Double] = InclusiveDefaultRange(0, 100)
  implicit val string: DefaultValue[String] = DefaultValue("")
  implicit def iterable[X]: DefaultValue[Iterable[X]] = DefaultValue(Iterable[X]())
  implicit def seq[X]: DefaultValue[Seq[X]] = DefaultValue(Seq[X]())
  implicit def map[X, Y]: DefaultValue[Map[X, Y]] = DefaultValue(Map[X, Y]())
}

/** Simple trait that represent a builder without build methods. Defines implicits for data validation*/
trait BuilderContent {
  protected implicit val int: DefaultRange[Int] = BuildersValidationImplicits.int
  protected implicit val double: DefaultRange[Double] = BuildersValidationImplicits.double
  protected implicit val string: DefaultValue[String] = BuildersValidationImplicits.string
  protected implicit def iterable[X]: DefaultValue[Iterable[X]] = BuildersValidationImplicits.iterable
  protected implicit def seq[X]: DefaultValue[Seq[X]] = BuildersValidationImplicits.seq
  protected implicit def map[X, Y]: DefaultValue[Map[X, Y]] = BuildersValidationImplicits.map
}

/** Rich trait that defines build methods that checks builder status only dynamically. Methods can build Partial or Complete
  * instance of requested data. Provides a base implementation for method build()
  *
  * @tparam P Type of partial data to build
  * @tparam C Type of complete data to build
  */
trait DynamicBuilder[+P, +C <: P] extends BuilderContent {
  /** Method tries to build a requested data's partial instance
    * @return A try of requested data
    */
  def tryBuild(): Try[P]
  /** Method tries to build a requested data's complete instance
    * @return A try of requested data
    */
  def tryCompleteBuild(): Try[C]

  /** Method build's a requested data's partial instance
    *
    * @throws exception if not possible
    * @return Partial data instance
    */
  @throws[Exception]
  def build(): P = {
    tryBuild() match {
      case Success(value) =>
        value
      case Failure(exception) =>
        throw exception
    }
  }
}

/**Trait that defines build methods that checks builder status only statically. Methods can build Complete
  * instance of requested data
  *
  * @tparam S Current builder status
  * @tparam CS Builder's complete status required to build complete instance
  * @tparam C Complete data's type
  */
trait StaticBuilder[S <: BuilderStatus, CS <: BuilderStatus, +C] extends BuilderContent {
  /** Method builds a complete instance of required data. Callable only at correct builder state
    *
    * @param ev Constraint to enable method only at correct status
    * @param st TypeTag of builder's current status
    * @return
    */
  def buildComplete(implicit ev: S =:= CS, st: TypeTag[S]): C
}

/** Rich trait that merges services defined by [[it.unibo.pps.ese.controller.simulation.loader.data.builder.StaticBuilder]]
  *  and [[it.unibo.pps.ese.controller.simulation.loader.data.builder.DynamicBuilder]], adding a base implementation
  * for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.StaticBuilder]] buildComplete()
  *
  * @tparam S Current builder status
  * @tparam CS Builder's complete status required to build complete instance
  * @tparam P Type of partial data to build
  * @tparam C Type of complete data to build
  */
trait GenericBuilder[S <: BuilderStatus, CS <: BuilderStatus, +P, +C <: P] extends DynamicBuilder[P, C] with StaticBuilder[S, CS, C] {
  override def buildComplete(implicit ev: S =:= CS, st: TypeTag[S]): C = {
    tryCompleteBuild() match {
      case Success(value) =>
        value
      case Failure(exception) =>
        throw exception
    }
  }
}

/** Rich interface that defines a base implementation for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.DynamicBuilder]]
  * tryBuild() using a template method
  *
  * @tparam S Current builder status
  * @tparam CS Builder's complete status required to build complete instance
  * @tparam P Type of partial data to build
  * @tparam C Type of complete data to build
  */
private[builder] trait BaseGenericBuilder[S <: BuilderStatus, CS <: BuilderStatus, +P, +C <: P] extends GenericBuilder[S, CS, P, C] {

  /** Create partial data instance
    *
    * @return Partial data instance
    */
  protected def buildPartialInstance(): P

  def tryBuild(): Try[P] = {
    tryCompleteBuild() match {
      case Success(value) =>
        Success(value)
      case Failure(_) =>
        Success(buildPartialInstance())
    }
  }
}

/** Rich trait that expands base definition of [[it.unibo.pps.ese.controller.simulation.loader.data.builder.BaseGenericBuilder]]
  * tryBuild() adding checks regarding builder's minimal status to build a partial data instance. Minimal status is both
  * defined by a given type and by dynamic method that checks mandatory fields validity
  *
  * @tparam S Current builder status
  * @tparam CS Builder's complete status required to build complete instance
  * @tparam P Type of partial data to build
  * @tparam C Type of complete data to build
  * @tparam VS Valid builder's status required to make a complete build
  */
private[builder] trait ValidStatusGenericBuilder[S <: BuilderStatus ,CS <: BuilderStatus, +P, +C <: P, VS <: BuilderStatus] extends GenericBuilder[S, CS, P, C] {
  /** Method checks mandatory fields required for a partial data build
    *
    * @return Exception representing mandatory fields missing or invalidity
    */
  protected def checkMandatoryProperties(): Option[CompleteBuildException]
  protected def status: TypeTag[S]
  protected def validStatus: TypeTag[VS]

  abstract override def tryBuild(): Try[P] = {
    super.tryBuild() match {
      case t: Success[_] =>
        if(!(status.tpe <:< validStatus.tpe && checkMandatoryProperties().isEmpty))
          Failure(checkMandatoryProperties().get)
        else
          t
      case t =>
        t
    }
  }
}

/** Base definition of builder's status*/
trait BuilderStatus
