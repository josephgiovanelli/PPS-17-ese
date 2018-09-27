package it.unibo.pps.ese.controller.simulation.loader.beans

/** Simple bean used for YAML deserialization*/
case class Gene(id: Option[String],
                simpleName: String,
                allelesPath: Option[String],
                properties: Option[Map[String, PropertyInfo]])
