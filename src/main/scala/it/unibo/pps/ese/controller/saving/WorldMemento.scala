package it.unibo.pps.ese.controller.saving

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Neocortex.NeocortexMemento
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.Hippocampus.HippocampusMemento
import it.unibo.pps.ese.genetics.SavedData

trait WorldMemento extends Memento {
  def isDefined: Boolean
}

object WorldMemento extends WorldMemento {
  var geneticsSimulatorMemento: Option[SavedData] = None

  override def isDefined: Boolean = {
    geneticsSimulatorMemento.isDefined
  }
}
