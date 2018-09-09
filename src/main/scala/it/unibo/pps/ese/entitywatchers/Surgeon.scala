package it.unibo.pps.ese.entitywatchers

import it.unibo.pps.ese.entitybehaviors.decisionsupport.SexTypes
import it.unibo.pps.ese.genericworld.model.{EntityInfoConversion, ReadOnlyEntityState}
import it.unibo.pps.ese.view.View

case class Surgeon(realTimeState: ReadOnlyEntityState) {

  var inspected: Option[String] = None

  def inspection(entityId: String): Unit =
    inspected = Some(entityId)

  def informAboutOrgansStatus(view: View): Unit = {
    if(inspected.isDefined) {
      import EntityInfoConversion._
      val entity = realTimeState getEntityState inspected.get
      view eyes entity.state.eyes
      view brain entity.state.hippocampus
      if (entity.state.gender == SexTypes.female) view pregnant entity.state.pregnant
      if (entity.state.pregnant) view embryo entity.state.embryo
      view reproductionOrgan entity.state.reproductionOrgan
    }
  }

}
