package it.unibo.pps.ese

import it.unibo.pps.ese.model._
import it.unibo.pps.ese.model.support.Done

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration._

object TestLauncher extends App {

    val world = World()
    val entity = Entity("improved", "1")
    val component = new ExampleComponent(entity specifications)
    entity addComponent component
    world addEntity entity

    while(true) {

        import EntityInfoConversion._

        val ret =
            for {
                a <- world.requireStateUpdate
                b <- world.requireInfoUpdate
            } yield b

        Await.result(ret, 500 millis )

        val b : Seq[EntityState] = world entitiesState;
        b filter (e => e.entityId == "1") foreach (e => println("Speed : " + e.state.speed))
        Thread.sleep(500)
    }
}
