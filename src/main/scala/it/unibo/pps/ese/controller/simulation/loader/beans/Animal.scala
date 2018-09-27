package it.unibo.pps.ese.controller.simulation.loader.beans

/** Simple bean used for YAML deserialization*/
case class Animal(name: String,
                  geneLength: Option[Int],
                  alleleLength: Option[Int],
                  reign: Option[String],
                  typology: Option[String],
                  structuralChromosome: Option[String],
                  regulationChromosome: Option[DefaultChromosomeData],
                  sexualChromosome: Option[DefaultChromosomeData])
