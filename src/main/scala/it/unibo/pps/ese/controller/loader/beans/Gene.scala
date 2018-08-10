package it.unibo.pps.ese.controller.loader.beans

case class Gene(id: String, simpleName: String, allelesPath: String, properties: Map[String, PropertyInfo])
