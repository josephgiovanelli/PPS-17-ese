package it.unibo.pps.ese.model

import scala.language.implicitConversions

object EntityInfoConversion {
  implicit class Ten(obj: Any) {
    def ten = 10
  }
}
