package it.unibo.pps.ese.genericworld

import java.util.UUID.randomUUID

import it.unibo.pps.ese.genericworld.controller.{Controller, View}
import it.unibo.pps.ese.genericworld.model._

object TestLauncher extends App {

    val world = World()
    val entity = Entity("improved", randomUUID().toString)
    val component = new ExampleComponent(entity specifications)
    entity addComponent component
    world addEntity entity

    val view = new View

    val controller = Controller(world, view)
    controller initialize 30

    controller.play()
    println("Playing")
    Thread.sleep(1000)

    controller.pause()
    println("Paused")
    Thread.sleep(1000)

    controller.play()

    println("Playing")
    Thread.sleep(1000)

    println("Stopped")
    controller.exit()
}
