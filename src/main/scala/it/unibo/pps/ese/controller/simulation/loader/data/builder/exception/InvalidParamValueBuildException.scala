package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

trait InvalidParamValueBuildException extends CompleteBuildException

object InvalidParamValueBuildException {
  def apply[X](paramName: String, paramValue: X): InvalidParamValueBuildException = {
    new AbsHierarchyMotivationsException("Param " + paramName + "cannot be " + paramValue)
      with InvalidParamValueBuildException
  }
}
