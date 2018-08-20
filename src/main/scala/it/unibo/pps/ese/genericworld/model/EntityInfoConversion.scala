package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityKinds
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityKinds.EntityKinds
import it.unibo.pps.ese.utils.Point

import scala.language.implicitConversions

object EntityInfoConversion {

  implicit class ExampleComponentConversions(obj: EntityInfo) {
    //def speed : Int = obj.selectDynamic("speed").asInstanceOf[Int]
  }

  implicit class BrainComponentConversions(obj: EntityInfo) {
    def position : Point = obj.selectDynamic("position").asInstanceOf[Point]
    def kind : EntityKinds = EntityKinds(Symbol(obj.selectDynamic("kind").asInstanceOf[String]))
    def height : Int = obj.selectDynamic("height").asInstanceOf[Int]
    def strong : Int = obj.selectDynamic("strong").asInstanceOf[Int]
    def defense : Int = obj.selectDynamic("defense").asInstanceOf[Int]
    def actionField : Int = obj.selectDynamic("actionField").asInstanceOf[Int]
    def visualField : Int = obj.selectDynamic("visualField").asInstanceOf[Int]
  }

  implicit class PhysicalStatusConversions(obj: EntityInfo) {
    def averageLife : Int = obj.selectDynamic("averageLife").asInstanceOf[Int]
    def energyRequirements : Int = obj.selectDynamic("energyRequirements").asInstanceOf[Int]
    def nutritiveValue : Int = obj.selectDynamic("nutritiveValue").asInstanceOf[Int]
    def endChildPhase : Int = obj.selectDynamic("endChildPhase").asInstanceOf[Int]
    def endAdultPhase : Int = obj.selectDynamic("endAdultPhase").asInstanceOf[Int]
    def percentageDecay : Double = obj.selectDynamic("percentageDecay").asInstanceOf[Double]
    def speed : Int = obj.selectDynamic("speed").asInstanceOf[Int]
  }
}
