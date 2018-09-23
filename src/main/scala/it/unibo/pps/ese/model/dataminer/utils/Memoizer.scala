package it.unibo.pps.ese.model.dataminer.utils

import scala.collection.mutable

trait Memoizer {
  protected def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
    override def apply(key: I): O = this synchronized { getOrElseUpdate(key, f(key)) }
  }
}
