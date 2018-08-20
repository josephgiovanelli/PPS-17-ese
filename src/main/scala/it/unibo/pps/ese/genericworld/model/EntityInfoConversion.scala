package it.unibo.pps.ese.genericworld.model

import scala.language.implicitConversions

object EntityInfoConversion {
  implicit class ExampleComponentConversions(obj: EntityInfo) {
    def speed : Int = obj.selectDynamic("speed").asInstanceOf[Int]
  }


}
