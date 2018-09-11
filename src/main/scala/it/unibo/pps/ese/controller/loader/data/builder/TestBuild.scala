package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AlleleData
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder.GeneStatus
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder.GeneStatus._

import scala.reflect.runtime.universe._

object TestBuild extends App {
  println(GeneBuilder()
    .setId("")
    .setName("")
    .addAlleles(Set[AlleleData]())
    .addProperties(Map())
    .buildDefault)

  println(GeneBuilder()
    .setId("")
    .setName("")
    .addAlleles(Set[AlleleData]())
    .addProperties(Map())
    .addConversionMap(Map())
    .buildCustom)

  var l: Seq[GeneBuilder[_ <: GeneBuilder.GeneStatus]] = Seq()

  for(i <- 1 to 5) {
    var gb: GeneBuilder[_ <: GeneBuilder.GeneStatus.EmptyGene] = GeneBuilder()
    //if(Random.nextBoolean())
      gb = gb.setId("")
    //if(Random.nextBoolean())
      gb = gb.setName("")
    //if(Random.nextBoolean())
      gb = gb.addAlleles(Set[AlleleData]())
    //if(Random.nextBoolean())
      gb = gb.addProperties(Map())

    l = l :+ gb
  }

  l.foreach(checkStatus(_))

  def checkStatus[T <: GeneStatus](gb: GeneBuilder[T]): Unit = gb match {
    case tt if tt.status.tpe <:< typeOf[DefaultGene] =>
      println("Default Gene")
      gb.asInstanceOf[GeneBuilder[DefaultGene]].buildDefault
    case tt if tt.status.tpe <:< typeOf[EmptyGene] =>
      println("Empty Gene")
    case tt if tt.status.tpe <:< typeOf[GeneStatus] =>
      println("Gene Gene")
    case tt =>
      println(gb.status)
  }

  def testRet(): GeneBuilder[_] = {
    GeneBuilder()
      .setId("")
      .setName("")
      .addAlleles(Set[AlleleData]())
      .addProperties(Map())
  }
}
