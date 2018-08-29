package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import it.unibo.pps.ese.controller.loader.data.{CustomGeneData, DefaultGeneData}


case class AnimalBaseInfo(name: String, geneLength: Int, alleleLength: Int, reign: String, typology: String)
case class AnimalChromosomeInfo(structuralChromosome: Iterable[CustomGeneData], regulationChromosome: Iterable[DefaultGeneData],
                                sexualChromosome: Iterable[DefaultGeneData])
