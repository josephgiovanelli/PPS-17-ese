package it.unibo.pps.ese

import it.unibo.pps.ese.model.support.Event
import it.unibo.pps.ese.model.{Entity, ReaderComponent, World, WriterComponent}

object TestLauncher extends App {

    val world = World()
    val entity = Entity("improved", "1")

    val p1 = new ReaderComponent()
    val p2 = new WriterComponent()

    entity addComponent p1
    entity addComponent p2

    world addEntity entity

    p1.subscribe(event => println(event))
    p2.writeData(new Event("Hello World"))

}
