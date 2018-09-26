package it.unibo.pps.ese.model.components.animals.brain.cerebralcortex

/**
  * The memory type
  */
trait MemoryType

/**
  * Memory associated to a hunting event
  */
case object Hunting extends MemoryType

/**
  * Memory associated to a couple event
  */
case object Couple extends MemoryType
