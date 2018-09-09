package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.entitybehaviors.decisionsupport.SexTypes
import it.unibo.pps.ese.genericworld.model.{EntityInfoConversion, ReadOnlyEntityState}
import it.unibo.pps.ese.view.View

case class Sniffer(realTimeState: ReadOnlyEntityState) {

  var watched: Option[String] = None

  def watch(entityId: String): Unit =
    watched = Some(entityId)

  def informAboutOrgansStatus(view: View): Unit = {
    if(watched.isDefined) {
      import EntityInfoConversion._
      val entity = realTimeState getEntityState watched.get
      view eyes entity.state.eyes
      view brain entity.state.hippocampus
      if (entity.state.gender == SexTypes.female) view pregnant entity.state.pregnant
      if (entity.state.pregnant) view embryo entity.state.embryo
      view reproductionOrgan entity.state.reproductionOrgan

    }
  }

}
