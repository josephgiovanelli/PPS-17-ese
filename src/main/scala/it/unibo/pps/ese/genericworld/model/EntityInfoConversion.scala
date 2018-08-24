package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.entitybehaviors.decisionsupport.{EntityKinds, SexTypes}
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityKinds.EntityKinds
import it.unibo.pps.ese.utils.Point

import scala.language.implicitConversions

object EntityInfoConversion {

  implicit class ExampleComponentConversions(obj: EntityInfo) {
    def fakeSpeed : Int = obj.selectDynamic("fakeSpeed").asInstanceOf[Int]
  }

  implicit class BrainComponentConversions(obj: EntityInfo) {
    def position : Point = obj.selectDynamic("position").asInstanceOf[Point]
    def kind : EntityKinds = EntityKinds(Symbol(obj.selectDynamic("kind").asInstanceOf[String]))
    def height : Double = obj.selectDynamic("height").asInstanceOf[Double]
    def strong : Double = obj.selectDynamic("strong").asInstanceOf[Double]
    def defense : Double = obj.selectDynamic("defense").asInstanceOf[Double]
    def actionField : Double = obj.selectDynamic("actionField").asInstanceOf[Double]
    def visualField : Double = obj.selectDynamic("visualField").asInstanceOf[Double]
    def attractiveness : Double = obj.selectDynamic("attractiveness").asInstanceOf[Double]
    def gender : SexTypes.Value = SexTypes.withNameOpt(obj.selectDynamic("gender").asInstanceOf[String]).get
  }

  implicit class PhysicalStatusConversions(obj: EntityInfo) {
    def averageLife : Double = obj.selectDynamic("averageLife").asInstanceOf[Double]
    def energyRequirements : Double = obj.selectDynamic("energyRequirements").asInstanceOf[Double]
    def nutritiveValue : Double = obj.selectDynamic("nutritiveValue").asInstanceOf[Double]
    def endChildPhase : Double = obj.selectDynamic("endChildPhase").asInstanceOf[Double]
    def endAdultPhase : Double = obj.selectDynamic("endAdultPhase").asInstanceOf[Double]
    def percentageDecay : Double = obj.selectDynamic("percentageDecay").asInstanceOf[Double]
    def speed : Double = obj.selectDynamic("speed").asInstanceOf[Double]
    def fertility: Double = obj.selectDynamic("fertility").asInstanceOf[Double]
  }

  implicit class BaseInfoConversions(obj: EntityInfo) {
    def species : String = obj.selectDynamic("species").asInstanceOf[String]
    def reign : String = obj.selectDynamic("reign").asInstanceOf[String]
  }
}
