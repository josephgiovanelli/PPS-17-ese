package it.unibo.pps.ese.controller.loader.beans

case class Plant(name: String,
                 geneLength: Option[Int],
                 alleleLength: Option[Int],
                 reign: Option[String],
                 height: Option[Double],
                 hardness: Option[Double],
                 nutritionalValue: Option[Double])
