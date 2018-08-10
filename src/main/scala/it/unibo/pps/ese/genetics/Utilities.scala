package it.unibo.pps.ese.genetics

import scala.util.Random

object Utilities {
  def pickRandomElement[T](a1: T,a2: T):T = Random.nextInt(2) match {
    case 0 => a1
    case 1 => a2
  }
  final def sample[A](dist: Map[A, Double]): A = {
    val p = scala.util.Random.nextDouble
    val it = dist.iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        return item  // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen")  // needed so it will compile
  }

  def softmax(seq:Seq[Double]):Seq[Double]= {
    val expSum = seq.map(math.exp).sum
    seq.map(math.exp(_)/expSum)
  }
}
