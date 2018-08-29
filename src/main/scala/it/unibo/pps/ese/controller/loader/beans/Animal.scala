package it.unibo.pps.ese.controller.loader.beans

case class Animal(name: String,
                  geneLength: Int,
                  alleleLength: Int,
                  reign: String,
                  typology: String,
                  structuralChromosome: String,
                  regulationChromosome: DefaultChromosomeData,
                  sexualChromosome: DefaultChromosomeData)