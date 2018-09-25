package it.unibo.pps.ese.controller.simulation.runner.incarnation

import it.unibo.pps.ese.controller.simulation.runner.core.EntityUpdateState
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityInfo
import it.unibo.pps.ese.model.components.animals.LifePhases
import it.unibo.pps.ese.model.components.animals.brain.ActionTypes
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityKinds.EntityKinds
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.{EntityKinds, GenderTypes}
import it.unibo.pps.ese.model.components.animals.trackers.EmbryoStatus
import it.unibo.pps.ese.utils.Point

/**
  * This implicit classes are used to enrich the EntityInfo class and obtain strong typed data from it.
  * This is necessary because on dynamic objects data is stored under Object interface.
  */
object EntityInfoConversion {

  implicit class ExampleComponentConversions(obj: EntityInfo) {
    def fakeSpeed : Int = obj.selectDynamic("fakeSpeed").asInstanceOf[Int]
  }

  implicit class ExtraFieldsConversions(obj: EntityInfo) {
    def status : EntityUpdateState.Value = obj.selectDynamic("status").asInstanceOf[EntityUpdateState.Value]
  }

  implicit class BrainComponentConversions(obj: EntityInfo) {
    def strength : Double = obj.selectDynamic("strength").asInstanceOf[Double]
    def actionField : Double = obj.selectDynamic("actionField").asInstanceOf[Double]
    def visualField : Double = obj.selectDynamic("visualField").asInstanceOf[Double]
    def attractiveness : Double = obj.selectDynamic("attractiveness").asInstanceOf[Double]
    def will: ActionTypes = obj.selectDynamic("will").asInstanceOf[ActionTypes]
  }

  implicit class PhysicalStatusComponentConversions(obj: EntityInfo) {
    def averageLife : Double = obj.selectDynamic("averageLife").asInstanceOf[Double]
    def energyRequirements : Double = obj.selectDynamic("energyRequirements").asInstanceOf[Double]
    def endChildPhase : Double = obj.selectDynamic("endChildPhase").asInstanceOf[Double]
    def endAdultPhase : Double = obj.selectDynamic("endAdultPhase").asInstanceOf[Double]
    def percentageDecay : Double = obj.selectDynamic("percentageDecay").asInstanceOf[Double]
    def speed : Double = obj.selectDynamic("speed").asInstanceOf[Double]
    def fertility: Double = obj.selectDynamic("fertility").asInstanceOf[Double]
    def age: Integer = obj.selectDynamic("age").asInstanceOf[Integer]
    def energy: Double = obj.selectDynamic("energy").asInstanceOf[Double]
    def lifePhase: LifePhases.Value = obj.selectDynamic("lifePhase").asInstanceOf[LifePhases.Value]
    def actualSpeed: Double = obj.selectDynamic("actualSpeed").asInstanceOf[Double]
    def actualFertility: Double = obj.selectDynamic("actualFertility").asInstanceOf[Double]
  }

  implicit class BaseInfoConversions(obj: EntityInfo) {
    def species : EntityKinds = EntityKinds(Symbol(obj.selectDynamic("species").asInstanceOf[String]))
    def reign : ReignType.Value = obj.selectDynamic("reign").asInstanceOf[ReignType.Value]
    def position : Point = (obj selectDynamic "position").asInstanceOf[Point]
    def height : Double = obj.selectDynamic("height").asInstanceOf[Double]
    def nutritionalValue : Double = obj.selectDynamic("nutritionalValue").asInstanceOf[Double]
    def defense : Double = obj.selectDynamic("defense").asInstanceOf[Double]
    def gender : GenderTypes.Value = GenderTypes.withNameOpt(obj.selectDynamic("gender").asInstanceOf[String]).get
    def baseEntityInfo : it.unibo.pps.ese.model.genetics.entities.EntityInfo = obj.selectDynamic("entityInfo").asInstanceOf[it.unibo.pps.ese.model.genetics.entities.EntityInfo]
  }

  implicit class OrgansTrackerComponentConversion(obj: EntityInfo) {
    def eyes: Boolean = obj.selectDynamic("eyes").asInstanceOf[Boolean]
    def hippocampus: Boolean = obj.selectDynamic("hippocampus").asInstanceOf[Boolean]
    def stomach: Boolean = obj.selectDynamic("stomach").asInstanceOf[Boolean]
    def pregnant: Boolean = obj.selectDynamic("pregnant").asInstanceOf[Boolean]
    def embryo : Option[EmbryoStatus.Value] = obj.selectDynamic("embryo").asInstanceOf[Option[EmbryoStatus.Value]]
    def reproductionOrgan: Boolean = obj.selectDynamic("reproductionOrgan").asInstanceOf[Boolean]
    //da levare
    def genes: Seq[String] = obj.selectDynamic("genes").asInstanceOf[Seq[String]]
  }

  implicit class InteractionTrackerComponentConversion(obj: EntityInfo) {
    def eat: Seq[String] = obj.selectDynamic("eat").asInstanceOf[Seq[String]]
    def couple: Seq[String] = obj.selectDynamic("couple").asInstanceOf[Seq[String]]
    def create: Seq[String] = obj.selectDynamic("create").asInstanceOf[Seq[String]]
  }
}
