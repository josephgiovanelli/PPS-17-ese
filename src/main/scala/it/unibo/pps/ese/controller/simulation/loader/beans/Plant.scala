package it.unibo.pps.ese.controller.simulation.loader.beans

/** Simple bean used for YAML deserialization*/
case class Plant(name: String,
                 geneLength: Option[Int],
                 alleleLength: Option[Int],
                 reign: Option[String],
                 height: Option[Double],
                 hardness: Option[Double],
                 nutritionalValue: Option[Double])
