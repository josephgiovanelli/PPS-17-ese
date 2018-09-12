package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Animal

trait AnimalData[C<:CustomGeneData, D<:DefaultGeneData] extends EntityData {
  def typology: String
  def structuralChromosome: Set[C]
  def regulationChromosome: Set[D]
  def sexualChromosome: Set[D]
}

object AnimalData {
  type PartialAnimalData = AnimalData[_ <: PartialCustomGeneData, _ <: PartialDefaultGeneData]
  type CompleteAnimalData = AnimalData[_ <: CompleteCustomGeneData, _ <: CompleteDefaultGeneData]

  def apply[C<:CustomGeneData, D<:DefaultGeneData](animal: Animal, structuralChromosome: Iterable[C], regulationChromosome: Iterable[D],
            sexualChromosome: Iterable[D]): AnimalData[C, D] = {
    new AnimalDataImpl(animal.name,
      animal.geneLength,
      animal.alleleLength,
      animal.reign,
      animal.typology,
      structuralChromosome,
      regulationChromosome,
      sexualChromosome)
  }

  def buildtest[C<:CustomGeneData, D<:DefaultGeneData](animal: Animal, structuralChromosome: Iterable[C], regulationChromosome: Iterable[D],
                                                   sexualChromosome: Iterable[D]): AnimalData[C, D] = {
    new AnimalDataImpl(animal.name,
      animal.geneLength,
      animal.alleleLength,
      animal.reign,
      animal.typology,
      structuralChromosome,
      regulationChromosome,
      sexualChromosome)
  }

  def apply[C<:CustomGeneData, D<:DefaultGeneData](name: String,
            geneLength: Int,
            alleleLength: Int,
            reign: String,
            typology: String,
            structuralChromosome: Iterable[C],
            regulationChromosome: Iterable[D],
            sexualChromosome: Iterable[D]): AnimalData[C, D] =
    new AnimalDataImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome, sexualChromosome)

  private class AnimalDataImpl[C<:CustomGeneData, D<:DefaultGeneData](val name: String,
                               val geneLength: Int,
                               val alleleLength: Int,
                               val reign: String,
                               val typology: String,
                               _structuralChromosome: Iterable[C],
                               _regulationChromosome: Iterable[D],
                               _sexualChromosome: Iterable[D]) extends AnimalData[C, D] {
    val structuralChromosome: Set[C] = _structuralChromosome.toSet
    require(_structuralChromosome.size == structuralChromosome.size)
    val regulationChromosome: Set[D] = _regulationChromosome.toSet
    require(_regulationChromosome.size == regulationChromosome.size)
    val sexualChromosome: Set[D] = _sexualChromosome.toSet
    require(_sexualChromosome.size == sexualChromosome.size)
    require((structuralChromosome ++ regulationChromosome ++ sexualChromosome).forall(g => g.id.length == geneLength &&
      g.alleles.forall(a => a.id.length == alleleLength)))
  }
}
