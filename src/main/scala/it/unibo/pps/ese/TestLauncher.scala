package it.unibo.pps.ese

import it.unibo.pps.ese.model._

object TestLauncher extends App {

    val world = World()
    val entity = Entity("improved", "1")

    val p1 = new ReaderComponent("1") {
      override def initialize(): Unit = Unit
    }
    val p2 = new WriterComponent("1") {
      override def initialize(): Unit = Unit
    }

    val p3 = new ExampleComponent("1")

    entity addComponent p1
    entity addComponent p2
    entity addComponent p3

    world addEntity entity

    p1.subscribe(event => println(event))
    p3.publish(ExampleEvent(100))
    p2.publish(RequireEntitiesState("example"))

}
