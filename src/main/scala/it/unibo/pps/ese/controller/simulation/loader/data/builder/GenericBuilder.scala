package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.utils.{DefaultRange, DefaultValue, InclusiveDefaultRange}

import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._

trait NotBuildableBuilder[S <: BuilderStatus] {
  protected implicit val int: DefaultRange[Int] = InclusiveDefaultRange(0, 100)
  protected implicit val double: DefaultRange[Double] = InclusiveDefaultRange(0, 100)
  protected implicit val string: DefaultValue[String] = DefaultValue("")
  protected implicit def iterable[X]: DefaultValue[Iterable[X]] = DefaultValue(Iterable[X]())
  protected implicit def seq[X]: DefaultValue[Seq[X]] = DefaultValue(Seq[X]())
  protected implicit def map[X, Y]: DefaultValue[Map[X, Y]] = DefaultValue(Map[X, Y]())
}

trait GenericBuilder[S <: BuilderStatus ,CS <: BuilderStatus, P, C <: P] extends NotBuildableBuilder[S] {
  def build(): P
  def tryCompleteBuild(): Try[C]
  def buildComplete(implicit ev: S =:= CS, st: TypeTag[S]): C = {
    tryCompleteBuild() match {
      case Success(value) =>
        value
      case Failure(exception) =>
        throw exception
    }
  }
}

trait ValidStatusGenericBuilder[S <: BuilderStatus ,CS <: BuilderStatus, P, C <: P, VS] extends GenericBuilder[S, CS, P, C] {
  protected def checkMandatoryProperties(): Option[CompleteBuildException]
  protected def status: TypeTag[S]
  protected def validStatus: TypeTag[VS]

  abstract override def build(): P = {
    if(!(status.tpe <:< validStatus.tpe && checkMandatoryProperties().isEmpty)) {
      throw checkMandatoryProperties().get
    }
    super.build()
  }
}

trait BuilderStatus
