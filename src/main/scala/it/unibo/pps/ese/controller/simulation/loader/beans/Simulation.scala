package it.unibo.pps.ese.controller.simulation.loader.beans

/** Simple bean used for YAML deserialization*/
case class Simulation(animals: Option[Map[String, Int]],
                      plants: Option[Map[String, Int]])
