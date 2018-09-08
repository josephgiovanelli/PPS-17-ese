package it.unibo.pps.ese.controller.saving

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Neocortex.NeocortexMemento
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.Hippocampus.HippocampusMemento


object Memento {
  var neocortexMemento: Option[NeocortexMemento] = None
  var hippocampusMemento: Option[HippocampusMemento] = None

}
