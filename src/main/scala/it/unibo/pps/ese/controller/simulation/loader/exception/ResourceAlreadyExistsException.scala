package it.unibo.pps.ese.controller.simulation.loader.exception

import it.unibo.pps.ese.controller.simulation.loader.io.ExistingResource

case class ResourceAlreadyExistsException(existingResource: ExistingResource) extends Exception
