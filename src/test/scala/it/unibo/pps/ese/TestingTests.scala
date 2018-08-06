package it.unibo.pps.ese

import org.scalatest.FunSuite

class TestingTests extends FunSuite {
  test("TestClass.number") {
    val t = new TestClass(5)
    assert(t.number == 5)
  }
}
