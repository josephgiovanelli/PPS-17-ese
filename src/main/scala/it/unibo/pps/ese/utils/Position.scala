package it.unibo.pps.ese.utils

/**
  * Express the x and y coordinates relative to the world
  */
trait Position {

  /**
    *
    * @return the x coordinate
    */
  def x: Double

  /**
    *
    * @return the y coordinate
    */
  def y: Double

  /**
    *
    * @param that the given position to be compared
    * @return true if both the current position's x and y coordinates are >= of the given one
    */
  def >=&&(that: Position): Boolean

  /**
    *
    * @param that the given position to be compared
    * @return true if both the current position's x and y coordinates are <= of the given one
    */
  def <=&&(that: Position): Boolean

  /**
    *
    * @param that the given position to be compared
    * @return true if at least one of the current position's x or y coordinates is > of the given one
    */
  def >||(that: Position): Boolean

  /**
    *
    * @param that the given position to be compared
    * @return true if at least one of the current position's x or y coordinates is < of the given one
    */
  def <||(that: Position): Boolean

  /**
    * Sums the x and y coordinates of this position to the x and y of the given one
    *
    * @param value the given position to be added
    * @return the sum between the postion
    */
  def +(value: Double): Position

  /**
    * Subtracts to the x and y coordinates of this position the x and y of the given one
    *
    * @param value the given position to be subtracted
    * @return the difference between the postion
    */
  def -(value: Double): Position

  /**
    *
    * @param position the given position
    * @return the geometric dinstance between the two positions
    */
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

