package it.unibo.pps.ese.genericworld

import java.util.UUID.randomUUID

import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl
import it.unibo.pps.ese.entitybehaviors._
import it.unibo.pps.ese.genericworld.controller.{Controller, View}
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._
import it.unibo.pps.ese.utils.Point


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scala.concurrent.Await

object TestLauncher extends App {

    StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
    val worldRules: WorldRulesImpl = decisionsupport.WorldRulesImpl.WorldRulesImpl(3, (0, 5), Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
        Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))

    StaticRules.instance().setRules(worldRules)

    val world = World()

    val entity1 = Entity("improved", "1")
    val component3 = BrainComponent(entity1 specifications, Point(7, 6), 6, 6, 6, "herbivore", 5, 20)
    val component4 = PhysicalStatusComponent(entity1 specifications, 50, 4, 4, 10, 30, 0.3, 2)
    entity1 addComponent component3
    entity1 addComponent component4

    val entity4 = Entity("improved", "4")
    val component1 = BrainComponent(entity4 specifications, Point(3, 3), 10, 10, 10, "carnivorous", 5, 20)
    val component2 = PhysicalStatusComponent(entity4 specifications, 50, 4, 10, 10, 30, 0.3, 2)
    entity4 addComponent component1
    entity4 addComponent component2

    world addEntity entity4
    world addEntity entity1

    Await.result(world.requireInfoUpdate, 50 millis)

    val res = world entitiesState

    val view = new View

    val controller = Controller(world, clockPeriod = 50 millis)
    controller attachView (view, frameRate = 60)

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
