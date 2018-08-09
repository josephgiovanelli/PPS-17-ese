package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Animal

trait AnimalData{
  def name: String
  def geneLength: Int
  def reign: String
  def typology: String
  def structuralChromosome: Seq[CustomGeneData]
  def regulationChromosome: Seq[DefaultGeneData]
  def sexualChromosome: Seq[DefaultGeneData]
}

object AnimalData {
  def apply(animal: Animal, structuralChromosome: Seq[CustomGeneData], regulationChromosome: Seq[DefaultGeneData],
            sexualChromosome: Seq[DefaultGeneData]): AnimalData = {
    AnimalDataImpl(animal.name,
      animal.geneLength,
      animal.reign,
      animal.typology,
      structuralChromosome,
      regulationChromosome,
      sexualChromosome)
  }
  //TODO lenght check
  private case class AnimalDataImpl(name: String,
                                    geneLength: Int,
                                    reign: String,
                                    typology: String,
                                    structuralChromosome: Seq[CustomGeneData],
                                    regulationChromosome: Seq[DefaultGeneData],
                                    sexualChromosome: Seq[DefaultGeneData]) extends AnimalData
}
