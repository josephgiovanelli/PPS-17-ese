package it.unibo.pps.ese.controller.simulation.loader.beans

/** Simple bean used for YAML deserialization*/
case class DefaultChromosomeData(allelesPath: Option[String],
                                 names: Option[Map[String, String]])
