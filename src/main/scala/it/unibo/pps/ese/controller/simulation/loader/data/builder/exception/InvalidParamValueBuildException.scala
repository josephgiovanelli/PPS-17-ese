package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

/** [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]] specialization
  * that indicates problems with a value inside a builder
  */
trait InvalidParamValueBuildException extends CompleteBuildException

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.InvalidParamValueBuildException]]*/
object InvalidParamValueBuildException {

  /**
    * @param location Incorrect value's location
    * @param paramName Incorrect value's name
    * @param paramValue Incorrect value's value
    * @tparam X Incorrect value's type
    * @return New [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.InvalidParamValueBuildException]]
    */
  def apply[X](location: String, paramName: String, paramValue: X): InvalidParamValueBuildException = {
    new AbsHierarchyMotivationsException(location + " | param " + paramName + "cannot be " + paramValue)
      with InvalidParamValueBuildException
  }
}
