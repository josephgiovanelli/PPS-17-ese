package it.unibo.pps.ese.model.genetics

import it.unibo.pps.ese.model.genetics.entities.{Quality, QualityValueConstraints}
import org.scalatest.FunSuite
import it.unibo.pps.ese.model.genetics.entities.QualityType._
class TestQualities extends FunSuite{
  test("Test the instantiation of a given quality"){
    val speedValue:Double = 50
    val speed:Quality = Quality(speedValue,Speed)
    assert(speed.qualityValue==speedValue)

    assertThrows[IllegalArgumentException](Quality(-1,Speed))
    assertThrows[IllegalArgumentException](Quality(QualityValueConstraints.maxSpeed+1,Speed))
  }
}
