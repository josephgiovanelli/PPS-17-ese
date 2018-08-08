package it.unibo.pps.ese.genetics

import scala.util.Random

object Utilities {
  def pickRandomElement[T](a1: T,a2: T):T = Random.nextInt(2) match {
    case 0 => a1
    case 1 => a2
  }
}
