package it.unibo.pps.ese.controller.loader.beans

case class Animal(name: Option[String],
                  geneLength: Option[Int],
                  alleleLength: Option[Int],
                  reign: Option[String],
                  typology: Option[String],
                  structuralChromosome: Option[String],
                  regulationChromosome: Option[DefaultChromosomeData],
                  sexualChromosome: Option[DefaultChromosomeData])
