package it.unibo.pps.ese.controller.loader.exception

import it.unibo.pps.ese.controller.util.io.ExistingResource

case class ResourceAlreadyExistsException(existingResource: ExistingResource) extends Exception
