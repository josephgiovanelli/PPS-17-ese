package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.utils.DefaultValue

import scala.util.Try
import scala.reflect.runtime.universe._

trait GenericBuilder[S <: BuilderStatus ,CS <: BuilderStatus, P, C <: P] {
  def build(): P
  def tryCompleteBuild(): Try[C]
  def buildComplete(implicit ev: S =:= CS, st: TypeTag[S]): C

  protected implicit val int: DefaultValue[Int] = DefaultValue(Integer.MIN_VALUE)
  protected implicit val double: DefaultValue[Double] = DefaultValue(Double.MinValue)
  protected implicit val string: DefaultValue[String] = DefaultValue("")
  protected implicit def iterable[X]: DefaultValue[Iterable[X]] = DefaultValue(Iterable[X]())
  protected implicit def seq[X]: DefaultValue[Seq[X]] = DefaultValue(Seq[X]())
  protected implicit def map[X, Y]: DefaultValue[Map[X, Y]] = DefaultValue(Map[X, Y]())
}

trait BuilderStatus
