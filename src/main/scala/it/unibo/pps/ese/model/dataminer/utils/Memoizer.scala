package it.unibo.pps.ese.model.dataminer.utils

import scala.collection.mutable

/**
  * If mixed this Trait gives a class the instruments to implement memoized functions
  */
trait Memoizer {

  /**
    * Memoization utility, if the result has not already been calculated executes the input function
    * and caches the result, otherwise returns directly the cached result
    * @param f The function to execute
    * @tparam I The input type
    * @tparam O The output type
    * @return A mapping from input to output based on cache content
    */
  protected def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
    override def apply(key: I): O = this synchronized { getOrElseUpdate(key, f(key)) }
  }
}
