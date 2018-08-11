package it.unibo.pps.ese

import it.unibo.pps.ese.model._

object TestLauncher extends App {

    val world = World()
    val entity = Entity("improved", "1")

    val p1 = new ReaderComponent() {
      override def initialize(): Unit = Unit
    }
    val p2 = new WriterComponent() {
      override def initialize(): Unit = Unit
    }

    entity addComponent p1
    entity addComponent p2

    world addEntity entity

    p1.subscribe(event => println(event))
    p2.writeData(new RequireEntitiesState)

}
