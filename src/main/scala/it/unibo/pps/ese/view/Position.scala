package it.unibo.pps.ese.view

case class Position(x: Double, y: Double) {
  def >=(that: Position): Boolean = {
    x>=that.x && y>=that.y
  }

  def <=(that: Position): Boolean = {
    x<=that.x && y<=that.y
  }

  def +(value: Double): Position = {
    Position(x+value, y+value)
  }

  def -(value: Double): Position = {
    Position(x-value, y-value)
  }

  def |-|(position: Position): Double = {
    import Math._
    sqrt(pow(position.x-x, 2) + pow(position.y-y, 2))
  }
}