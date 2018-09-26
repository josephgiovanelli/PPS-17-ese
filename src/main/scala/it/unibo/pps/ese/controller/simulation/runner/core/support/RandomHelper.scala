package it.unibo.pps.ese.controller.simulation.runner.core.support

import it.unibo.pps.ese.utils.Point

import scala.util.Random

object RandomHelper {

  /**
    * This class enriches the functionalities of Random's companion object
    */
  implicit class PimpedRandom(companion: Random.type) {

    /**
      * Produce n distinct random points on a bidimensional space of dimension x * y
      * @param n Number of points to produce
      * @param x Space width
      * @param y Space height
      * @return Required points
      */
    def distinctRandomPoints(n:Int, x:Int, y:Int):Set[Point] = {
      import scala.util.Random
      require(n < x * y)
      Stream.continually((Random.nextInt(x), Random.nextInt(y))).scanLeft(Set[Point]()) {
        (accumulator, el) => accumulator + Point(el._1, el._2)
      }.dropWhile(_.size < n).head
    }
  }

}
