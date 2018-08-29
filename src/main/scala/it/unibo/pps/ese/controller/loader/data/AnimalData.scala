package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Animal

trait AnimalData extends EntityData {
  def typology: String
  def structuralChromosome: Set[CustomGeneData]
  def regulationChromosome: Set[DefaultGeneData]
  def sexualChromosome: Set[DefaultGeneData]
}

object AnimalData {
  def apply(animal: Animal, structuralChromosome: Iterable[CustomGeneData], regulationChromosome: Iterable[DefaultGeneData],
            sexualChromosome: Iterable[DefaultGeneData]): AnimalData = {
    new AnimalDataImpl(animal.name,
      animal.geneLength,
      animal.alleleLength,
      animal.reign,
      animal.typology,
      structuralChromosome,
      regulationChromosome,
      sexualChromosome)
  }

  def apply(name: String,
            geneLength: Int,
            alleleLength: Int,
            reign: String,
            typology: String,
            structuralChromosome: Iterable[CustomGeneData],
            regulationChromosome: Iterable[DefaultGeneData],
            sexualChromosome: Iterable[DefaultGeneData]): AnimalData =
    new AnimalDataImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome, sexualChromosome)

  private class AnimalDataImpl(val name: String,
                               val geneLength: Int,
                               val alleleLength: Int,
                               val reign: String,
                               val typology: String,
                               _structuralChromosome: Iterable[CustomGeneData],
                               _regulationChromosome: Iterable[DefaultGeneData],
                               _sexualChromosome: Iterable[DefaultGeneData]) extends AnimalData {
    val structuralChromosome: Set[CustomGeneData] = _structuralChromosome.toSet
    require(_structuralChromosome.size == structuralChromosome.size)
    val regulationChromosome: Set[DefaultGeneData] = _regulationChromosome.toSet
    require(_regulationChromosome.size == regulationChromosome.size)
    val sexualChromosome: Set[DefaultGeneData] = _sexualChromosome.toSet
    require(_sexualChromosome.size == sexualChromosome.size)
    require((structuralChromosome ++ regulationChromosome ++ sexualChromosome).forall(g => g.id.length == geneLength &&
      g.alleles.forall(a => a.id.length == alleleLength)))
  }
}
