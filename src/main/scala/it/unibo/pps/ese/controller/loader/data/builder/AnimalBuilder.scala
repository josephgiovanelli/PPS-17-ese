package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AnimalData
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus.{AnimalTemplate, AnimalWithGeneLength, AnimalWithName}
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder.GeneStatus

import scala.reflect.runtime.universe._

class AnimalBuilder[T <: AnimalStatus](name: String = null,
                                       geneLength: Int = 0,
                                       alleleLength: Int = 0,
                                       reign: String = null,
                                       typology: String = null,
                                       structuralChromosome: Iterable[GeneBuilder[_]] = Seq(),
                                       regulationChromosome: Iterable[GeneBuilder[_]] = Seq(),
                                       sexualChromosome: Iterable[GeneBuilder[_]] = Seq())
                                      (implicit val status: TypeTag[T]){
  //TODO use copy method?
  def setName(name: String): AnimalBuilder[T with AnimalWithName] =
    new AnimalBuilder(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
      sexualChromosome)

  def setGeneLength(geneLength: Int): AnimalBuilder[T with AnimalWithGeneLength] =
    new AnimalBuilder(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
      sexualChromosome)

  //def buildComplete(implicit ev: T =:= FullAnimal): AnimalData = ???

//  //TODO define animal template trait
//  def buildTemplate(implicit ev: T =:= AnimalTemplate): AnimalData = {
//    //TODO require dynamic status check
//    require(structuralChromosome.forall(g => g.status.tpe =:= typeOf[GeneStatus.CustomGeneTemplate]))
//    val l = structuralChromosome.map(c => c.asInstanceOf[GeneBuilder[GeneStatus.CustomGeneTemplate]].buildCustomTemplate)
//    AnimalData(name, geneLength, alleleLength, reign, typology, l, Seq(), Seq())
//  }
}

object AnimalBuilder {
  sealed trait AnimalStatus
  object AnimalStatus {
    sealed trait EmptyAnimal
    sealed trait AnimalWithName
    sealed trait AnimalWithGeneLength
    sealed trait AnimalWithAlleleLength
    sealed trait AnimalWithReign
    sealed trait AnimalWithTypology
    sealed trait AnimalWithStructChromosome
    sealed trait AnimalWithRegChromosome
    sealed trait AnimalWithSexChromosome

    type AnimalWithBaseInfo = EmptyAnimal with AnimalWithName with AnimalWithGeneLength with AnimalWithAlleleLength
      with  AnimalWithReign with AnimalWithTypology
    type AnimalTemplate = AnimalWithBaseInfo with AnimalWithStructChromosome
    type FullAnimal = AnimalTemplate with AnimalWithRegChromosome with AnimalWithSexChromosome
  }
}
