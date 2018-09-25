package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.utils.{DefaultRange, DefaultValue, InclusiveDefaultRange}

import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._

object BuildersValidationImplicits {
  implicit val int: DefaultRange[Int] = InclusiveDefaultRange(0, 100)
  implicit val double: DefaultRange[Double] = InclusiveDefaultRange(0, 100)
  implicit val string: DefaultValue[String] = DefaultValue("")
  implicit def iterable[X]: DefaultValue[Iterable[X]] = DefaultValue(Iterable[X]())
  implicit def seq[X]: DefaultValue[Seq[X]] = DefaultValue(Seq[X]())
  implicit def map[X, Y]: DefaultValue[Map[X, Y]] = DefaultValue(Map[X, Y]())
}

trait NotBuildableBuilder[S <: BuilderStatus] {
  implicit val int: DefaultRange[Int] = BuildersValidationImplicits.int
  implicit val double: DefaultRange[Double] = BuildersValidationImplicits.double
  implicit val string: DefaultValue[String] = BuildersValidationImplicits.string
  implicit def iterable[X]: DefaultValue[Iterable[X]] = BuildersValidationImplicits.iterable
  implicit def seq[X]: DefaultValue[Seq[X]] = BuildersValidationImplicits.seq
  implicit def map[X, Y]: DefaultValue[Map[X, Y]] = BuildersValidationImplicits.map
}

trait GenericBuilder[S <: BuilderStatus ,CS <: BuilderStatus, P, C <: P] extends NotBuildableBuilder[S] {
  def tryBuild(): Try[P]
  def tryCompleteBuild(): Try[C]
  def build(): P = {
    tryBuild() match {
      case Success(value) =>
        value
      case Failure(exception) =>
        throw exception
    }
  }
  def buildComplete(implicit ev: S =:= CS, st: TypeTag[S]): C = {
    tryCompleteBuild() match {
      case Success(value) =>
        value
      case Failure(exception) =>
        throw exception
    }
  }
}

trait BaseBuildableGenericBuilder[S <: BuilderStatus ,CS <: BuilderStatus, P, C <: P] extends GenericBuilder[S, CS, P, C] {

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

trait ValidStatusGenericBuilder[S <: BuilderStatus ,CS <: BuilderStatus, P, C <: P, VS] extends GenericBuilder[S, CS, P, C] {
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

trait BuilderStatus
