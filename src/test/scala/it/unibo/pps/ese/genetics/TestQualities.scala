package it.unibo.pps.ese.genetics

import org.scalatest.FunSuite
import QualityType._
class TestQualities extends FunSuite{
  test("Test the instantiation of a given quality"){
    val speedValue:Double = 50
    val speed:Quality = Quality(speedValue,Speed)
    assert(speed.qualityValue==speedValue)

    assertThrows[IllegalArgumentException](Quality(-1,Speed))
    assertThrows[IllegalArgumentException](Quality(QualityValueConstraints.maxSpeed+1,Speed))
  }
}
