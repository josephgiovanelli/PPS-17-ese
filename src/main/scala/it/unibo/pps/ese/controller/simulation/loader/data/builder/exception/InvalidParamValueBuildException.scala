package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

trait InvalidParamValueBuildException extends CompleteBuildException

object InvalidParamValueBuildException {
  def apply[X](location: String, paramName: String, paramValue: X): InvalidParamValueBuildException = {
    new AbsHierarchyMotivationsException(location + " | param " + paramName + "cannot be " + paramValue)
      with InvalidParamValueBuildException
  }
}
