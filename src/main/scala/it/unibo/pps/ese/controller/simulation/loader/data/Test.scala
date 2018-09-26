package it.unibo.pps.ese.controller.simulation.loader.data

import it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.GeneData.{CompleteGeneData, PartialGeneData}

import scala.reflect.runtime.universe._

object Test extends App {
  println(typeOf[CompleteGeneData] <:< typeOf[PartialGeneData])
  println(typeOf[CompleteDefaultGeneData] <:< typeOf[PartialDefaultGeneData])
  println(typeOf[PartialDefaultGeneData] <:< typeOf[PartialGeneData])
  println(typeOf[CompleteDefaultGeneData] <:< typeOf[CompleteGeneData])
  println(typeOf[FullDefaultGeneData[CompleteAlleleData]] <:< typeOf[CompleteGeneData])
  println(typeOf[FullDefaultGeneData[CompleteAlleleData]] <:< typeOf[FullGeneData[CompleteAlleleData]])
  println(typeOf[CompleteDefaultGeneData] <:< typeOf[FullGeneData[CompleteAlleleData]])


//  println(typeOf[CompleteGeneData] <:< typeOf[PartialGeneData])
//  println(typeOf[CompleteCustomGeneData] <:< typeOf[PartialCustomGeneData])
//  println(typeOf[PartialCustomGeneData] <:< typeOf[PartialGeneData])
//  println(typeOf[CompleteCustomGeneData] <:< typeOf[CompleteGeneData])

}
