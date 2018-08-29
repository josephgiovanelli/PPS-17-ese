package it.unibo.pps.ese.view

case class Position(var x: Double, var y: Double) {
  def >=(that: Position): Boolean = {
    x>=that.x && y>=that.y
  }

  def <=(that: Position): Boolean = {
    x<=that.x && y<=that.y
  }
}