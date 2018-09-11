package it.unibo.pps.ese.controller.saving

trait Memento

trait Savable[M <: Memento] {
  def serialize: M
}
