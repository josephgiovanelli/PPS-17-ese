package it.unibo.pps.ese.controller.saving

import java.io._
import java.nio.file.Paths
import java.util.UUID.randomUUID

import it.unibo.pps.ese.entitybehaviors.BrainComponent
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.Hippocampus
import it.unibo.pps.ese.genericworld.model.Entity

import scala.concurrent.ExecutionContext

object Saver extends App {

//  val fileName = "Prova"
//  val path = Paths.get(getClass.getResource("/it/unibo/pps/ese/controller/saves").toURI).toString + fileName
//
//  val entity = Entity("improved", randomUUID().toString)(ExecutionContext.Implicits.global)
//  val brainComponent = BrainComponent(entity.specifications, 1000, 1000, 5, 5, 5, 5)(ExecutionContext.Implicits.global)
//  val oos = new ObjectOutputStream(new FileOutputStream(path))
//  oos.writeObject(brainComponent)
//  oos.close
//
//  // (3) read the object back in
//  val ois = new ObjectInputStream(new FileInputStream(path))
//  val h = ois.readObject.asInstanceOf[Hippocampus]
//  ois.close
//
//  println(h)
}
