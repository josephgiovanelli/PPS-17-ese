package it.unibo.pps.ese.controller.simulation.loader.exception

import it.unibo.pps.ese.controller.simulation.loader.io.ExistingResource

/** Class that defines a custom exception for data saving. In particular exception regards an already existing resource
  * found during saving process
  *
  * @param existingResource Already existing resource
  */
case class ResourceAlreadyExistsException(existingResource: ExistingResource)
  extends Exception("Resource: " + existingResource + " already exists")
