package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AnimalData.{AnimalDataImpl, CompleteAnimalData}
import it.unibo.pps.ese.controller.loader.data.{CompleteCustomGeneData, CompleteDefaultGeneData, PartialCustomGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus
import it.unibo.pps.ese.controller.loader.data.builder.AnimalBuilder.AnimalStatus._

import scala.reflect.runtime.universe._

trait AnimalBuilder[T <: AnimalStatus] {

}

object AnimalBuilder {

  class AnimalBuilderImpl[T <: AnimalStatus](name: String = null,
                                             geneLength: Int = 0,
                                             alleleLength: Int = 0,
                                             reign: String = null,
                                             typology: String = null,
                                             structuralChromosome: Iterable[_ <: PartialCustomGeneData] = Seq(),
                                             regulationChromosome: Iterable[_ <: PartialDefaultGeneData] = Seq(),
                                             sexualChromosome: Iterable[_ <: PartialDefaultGeneData] = Seq())
                                            (implicit val status: TypeTag[T]) extends AnimalBuilder[T]{
    //TODO use copy method?
    def setName(name: String): AnimalBuilder[T with AnimalWithName] =
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

    def setGeneLength(geneLength: Int): AnimalBuilder[T with AnimalWithGeneLength] =
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

    def setAlleleLength(alleleLength: Int): AnimalBuilder[T with AnimalWithAlleleLength] =
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

    def setReign(reign: String): AnimalBuilder[T with AnimalWithReign] =
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

    def setTypology(typology: String): AnimalBuilder[T with AnimalWithTypology] =
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome)

    def addStructuralChromosome(structuralChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithStructChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome.map(_.buildCustom), regulationChromosome,
        sexualChromosome)
    }

    def addRegulationChromosome(regulationChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithRegChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome.map(_.buildDefault),
        sexualChromosome)
    }

    def addSexualChromosome(sexualChromosome: Iterable[GeneBuilder[_]]): AnimalBuilder[T with AnimalWithSexChromosome] = {
      new AnimalBuilderImpl(name, geneLength, alleleLength, reign, typology, structuralChromosome, regulationChromosome,
        sexualChromosome.map(_.buildDefault))
    }

    def buildComplete(implicit ev: T =:= FullAnimal): CompleteAnimalData = {
      val struct: Iterable[CompleteCustomGeneData] = structuralChromosome.flatMap({
        case c: CompleteCustomGeneData =>
          Some(c)
        case _ =>
          None
      })
      require(struct.size == structuralChromosome.size)
      val reg: Iterable[CompleteDefaultGeneData] = regulationChromosome.flatMap({
        case c: CompleteDefaultGeneData =>
          Some(c)
        case _ =>
          None
      })
      require(reg.size == regulationChromosome.size)
      val sex: Iterable[CompleteDefaultGeneData] = sexualChromosome.flatMap({
        case c: CompleteDefaultGeneData =>
          Some(c)
        case _ =>
          None
      })
      require(sex.size == sexualChromosome.size)
      new AnimalDataImpl(name, geneLength, alleleLength, reign, typology, struct, reg, sex)
    }

    //  //TODO define animal template trait
    //  def buildTemplate(implicit ev: T =:= AnimalTemplate): AnimalData = {
    //    //TODO require dynamic status check
    //    require(structuralChromosome.forall(g => g.status.tpe =:= typeOf[GeneStatus.CustomGeneTemplate]))
    //    val l = structuralChromosome.map(c => c.asInstanceOf[GeneBuilder[GeneStatus.CustomGeneTemplate]].buildCustomTemplate)
    //    AnimalData(name, geneLength, alleleLength, reign, typology, l, Seq(), Seq())
    //  }
  }

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
