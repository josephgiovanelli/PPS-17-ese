package it.unibo.pps.ese.controller.simulation.loader.data.builder

object Test extends App {
  trait A {
    def print = "helloA"
  }

  class B extends A {
    override def print = "helloB"
  }

  trait C extends A {
    override def print = super.print + " helloC"
  }

  class D extends B with C

  val v = new B with C
  println(v.print)
  println((new D).print)
}
