package it.unibo.pps.ese.controller.loader.data.builder

import scala.util.Try
import scala.reflect.runtime.universe._

trait GenericBuilder[S <: BuilderStatus ,CS <: BuilderStatus, P, C <: P] {
  def build(): P
  def tryCompleteBuild(): Try[C]
  def buildComplete(implicit ev: S =:= CS, st: TypeTag[S]): C
}

trait BuilderStatus
