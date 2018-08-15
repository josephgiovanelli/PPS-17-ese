package it.unibo.pps.ese.view

import it.unibo.pps.ese.view.WorldPrefernces.{worldHeigth, worldWidth}
import scalafx.scene.paint.Color

import scala.util.Random

trait Observer {
  def getEntityDetails(position: Position): EntityDetails
}

trait EntityDetails

class Controller(val view: View) extends Observer {

  view.addObserver(this)

  val r: Random = Random
  view.updateWorld(1, (0 until 3000)
    .map(x => Entity(
      name = "a"+x,
      color = Color.Green,
      position = Position(r.nextInt(worldWidth), r.nextInt(worldHeigth))))
    .toList)

  override def getEntityDetails(position: Position): EntityDetails = ???
}
