package it.unibo.pps.ese.controller.simulation.loader.beans

case class Simulation(animals: Option[Map[String, Int]],
                      plants: Option[Map[String, Int]])
