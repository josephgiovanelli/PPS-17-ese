package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.entitybehaviors.ActionKind.ActionKind
import it.unibo.pps.ese.entitybehaviors.{ActionKind, EmbryoStatus, LifePhases}
import it.unibo.pps.ese.entitybehaviors.decisionsupport.{EntityKinds, SexTypes}
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityKinds.EntityKinds
import it.unibo.pps.ese.utils.Point

import scala.language.implicitConversions

object EntityInfoConversion {

  implicit class ExampleComponentConversions(obj: EntityInfo) {
    def fakeSpeed : Int = obj.selectDynamic("fakeSpeed").asInstanceOf[Int]
  }

  implicit class ExtraFieldsConversions(obj: EntityInfo) {
    def status : EntityUpdateState.Value = obj.selectDynamic("status").asInstanceOf[EntityUpdateState.Value]
  }

  implicit class BrainComponentConversions(obj: EntityInfo) {
    def strong : Double = obj.selectDynamic("strong").asInstanceOf[Double]
    def actionField : Double = obj.selectDynamic("actionField").asInstanceOf[Double]
    def visualField : Double = obj.selectDynamic("visualField").asInstanceOf[Double]
    def attractiveness : Double = obj.selectDynamic("attractiveness").asInstanceOf[Double]
    def will: ActionKind.Value = obj.selectDynamic("will").asInstanceOf[ActionKind.Value]
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
    def gender : SexTypes.Value = SexTypes.withNameOpt(obj.selectDynamic("gender").asInstanceOf[String]).get
    def baseEntityInfo : it.unibo.pps.ese.genetics.entities.EntityInfo = obj.selectDynamic("entityInfo").asInstanceOf[it.unibo.pps.ese.genetics.entities.EntityInfo]
  }

  implicit class PlantPhysicalComponentConversions(obj: EntityInfo) {
    def availability: Double = obj.selectDynamic("availability").asInstanceOf[Double]
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
