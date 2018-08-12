package it.unibo.pps.ese

import it.unibo.pps.ese.model._

object TestLauncher extends App {

    val world = World()
    val entity = Entity("improved", "1")

    val p1 = new ReaderComponent(entity specifications) {
      override def initialize(): Unit = Unit
    }
    val p2 = new WriterComponent(entity specifications) {
      override def initialize(): Unit = Unit
    }

    val p3 = new ExampleComponent(entity specifications)

    entity addComponent p1
    entity addComponent p2
    entity addComponent p3

    world addEntity entity

    p1.subscribe(event => println(event))
    p3.publish(ExampleEvent(100))
    p3.publish(RequireEntitiesState("example"))

}
