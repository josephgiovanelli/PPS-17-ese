package it.unibo.pps.ese.entitybehaviors.cerebralCortex

sealed trait Position {

  def x: Double
  def y: Double

  def >=&&(that: Position): Boolean
  def <=&&(that: Position): Boolean
  def >||(that: Position): Boolean
  def <||(that: Position): Boolean
  def +(value: Double): Position
  def -(value: Double): Position
  def |-|(position: Position): Double
}

object Position {

  def apply(x: Double, y: Double): Position = new PositionImpl(x, y)
  def unapply(position: Position): Option[(Double, Double)] = Some(position.x, position.y)

  case class PositionImpl(x: Double, y: Double) extends Position {
    def >=&&(that: Position): Boolean = {
      x>=that.x && y>=that.y
    }

    def <=&&(that: Position): Boolean = {
      x<=that.x && y<=that.y
    }

    def >||(that: Position): Boolean = {
      x>that.x || y>that.y
    }

    def <||(that: Position): Boolean = {
      x<that.x || y<that.y
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
}

