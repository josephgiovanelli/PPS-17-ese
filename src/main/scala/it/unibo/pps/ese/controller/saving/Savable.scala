package it.unibo.pps.ese.controller.saving

trait Savable[T] {
  def serialize: T
}
