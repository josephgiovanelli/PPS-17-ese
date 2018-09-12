package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Animal

trait AnimalData[C<:PartialCustomGeneData, D<:PartialDefaultGeneData] extends EntityData {
  def getTypology: Option[String]
  def getStructuralChromosome: Option[Iterable[C]]
  def getRegulationChromosome: Option[Iterable[D]]
  def getSexualChromosome: Option[Iterable[D]]
}

trait FullAnimalData[C<:PartialCustomGeneData, D<:PartialDefaultGeneData] extends AnimalData[C, D] with FullEntityData{
  def typology: String = getTypology.getOrElse(throw new IllegalStateException())
  def structuralChromosome: Set[C] = getStructuralChromosome.getOrElse(throw new IllegalStateException()).toSet
  def regulationChromosome: Set[D] = getRegulationChromosome.getOrElse(throw new IllegalStateException()).toSet
  def sexualChromosome: Set[D] = getSexualChromosome.getOrElse(throw new IllegalStateException()).toSet
}

object AnimalData {
  type PartialAnimalData = AnimalData[_ <: PartialCustomGeneData, _ <: PartialDefaultGeneData]
  type CompleteAnimalData = FullAnimalData[_ <: CompleteCustomGeneData, _ <: CompleteDefaultGeneData]

  def apply[C<:PartialCustomGeneData, D<:PartialDefaultGeneData](animal: Animal, structuralChromosome: Iterable[C], regulationChromosome: Iterable[D],
            sexualChromosome: Iterable[D]): FullAnimalData[C, D] = {
    new AnimalDataImpl(animal.name,
      animal.geneLength,
      animal.alleleLength,
      animal.reign,
      animal.typology,
      structuralChromosome,
      regulationChromosome,
      sexualChromosome) with FullAnimalData[C, D]
  }

  def buildtest[C<:PartialCustomGeneData, D<:PartialDefaultGeneData](animal: Animal, structuralChromosome: Iterable[C], regulationChromosome: Iterable[D],
                                                   sexualChromosome: Iterable[D]): FullAnimalData[C, D] = {
    new AnimalDataImpl(animal.name,
      animal.geneLength,
      animal.alleleLength,
      animal.reign,
      animal.typology,
      structuralChromosome,
      regulationChromosome,
      sexualChromosome) with FullAnimalData[C, D]
  }

  class AnimalDataImpl[C<:PartialCustomGeneData, D<:PartialDefaultGeneData](_name: String,
                                                                            _geneLength: Int,
                                                                            _alleleLength: Int,
                                                                            _reign: String,
                                                                            _typology: String,
                                                                            _structuralChromosome: Iterable[C],
                                                                            _regulationChromosome: Iterable[D],
                                                                            _sexualChromosome: Iterable[D]) extends AnimalData[C, D] {
//    val structuralChromosome: Set[C] = _structuralChromosome.toSet
//    require(_structuralChromosome.size == structuralChromosome.size)
//    val regulationChromosome: Set[D] = _regulationChromosome.toSet
//    require(_regulationChromosome.size == regulationChromosome.size)
//    val sexualChromosome: Set[D] = _sexualChromosome.toSet
//    require(_sexualChromosome.size == sexualChromosome.size)
//    require((structuralChromosome ++ regulationChromosome ++ sexualChromosome).forall(g => g.id.length == geneLength &&
//      g.alleles.forall(a => a.id.length == alleleLength)))
    override val getTypology: Option[String] = Some(_typology)

    override val getStructuralChromosome: Option[Iterable[C]] = Some(_structuralChromosome)

    override val getRegulationChromosome: Option[Iterable[D]] = Some(_regulationChromosome)

    override val getSexualChromosome: Option[Iterable[D]] = Some(_sexualChromosome)

    override val getName: Option[String] = Some(_name)

    override val getGeneLength: Option[Int] = Some(_geneLength)

    override val getAlleleLength: Option[Int] = Some(_alleleLength)

    override val getReign: Option[String] = Some(_reign)
  }
}
