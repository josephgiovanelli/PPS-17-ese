package it.unibo.pps.ese.model.genetics

import it.unibo.pps.ese.model.genetics.dna._
import it.unibo.pps.ese.model.genetics.entities.{Female, Male}
import org.scalatest.FunSuite

class TestChromosome extends FunSuite{
  test("Test the creation of various chromosome"){
    val gs= GeneWithAllelicForms(List('A'),List('A'),RegulatorGene)
    val gs2 = GeneWithAllelicForms(List('A'),List('F'),RegulatorGene)


    val g1p = BasicGene(List('F'),IdentifierGene)
    val g2p = BasicGene(List('A'),IdentifierGene)
    val c1p = Chromosome(ChromosomeType.COMMON,g1p,g2p)
    val c2p= Chromosome(ChromosomeType.COMMON,g1p,g2p)

    assert(c1p.geneList == List(g1p,g2p))


    assertThrows[IllegalArgumentException](Chromosome(ChromosomeType.COMMON,g1p))

    val gs1p = GeneWithAllelicForms(List('F'),List('A'),StructuralGene)
    val gs2p = GeneWithAllelicForms(List('F'),List('F'),StructuralGene)
    val gs3p = GeneWithAllelicForms(List('F'),List('D'),StructuralGene)

    val cs1p = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL,gs1p,gs2p,gs3p)
    val cs2p = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL,gs1p,gs2p,gs3p)

    assert(cs1p.geneList==List(gs1p,gs2p,gs3p))
    val g3p = BasicGene(List('A'),IdentifierGene)
    assertThrows[IllegalArgumentException](Chromosome(ChromosomeType.STRUCTURAL_ANIMAL,g3p))


    val sgr1 = GeneWithAllelicForms(List('A'),List('A'),RegulatorGene)
    val sgr2 = GeneWithAllelicForms(List('A'),List('F'),RegulatorGene)
    val sgr3 = GeneWithAllelicForms(List('A'),List('G'),RegulatorGene)

    val cs1 = Chromosome(ChromosomeType.SEXUAL_X,X,sgr1,sgr2,sgr3)
    val cs2 = Chromosome(ChromosomeType.SEXUAL_X,X,sgr1,sgr2,sgr3)
    assert(cs1.geneList==List(sgr1,sgr2,sgr3))

    assertThrows[IllegalArgumentException](Chromosome(ChromosomeType.SEXUAL_Y,sgr1))
  }

  test("Test the basic operation of Chromosome couples"){
    val g1p = BasicGene(List('F'),IdentifierGene)
    val g2p = BasicGene(List('A'),IdentifierGene)
    val c1p:Chromosome = Chromosome(ChromosomeType.COMMON,g1p,g2p)
    val c2p:Chromosome = Chromosome(ChromosomeType.COMMON,g1p,g2p)

    val cc1 = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    cc1.addChromosomeCouple(c1p,c2p)
    assert(c1p == cc1.firstChromosome)
    assert(c2p == cc1.secondChromosome)

    cc1.addFirstChromosome(c2p)
    assert(c2p == cc1.firstChromosome)
    cc1.addSecondChromosome(c1p)
    assert(c1p == cc1.secondChromosome)


    val sgr1 = GeneWithAllelicForms(List('A'),List('A'),RegulatorGene)
    val sgr2 = GeneWithAllelicForms(List('A'),List('F'),RegulatorGene)
    val sgr3 = GeneWithAllelicForms(List('A'),List('G'),RegulatorGene)

    val cs1 = Chromosome(ChromosomeType.SEXUAL_X,X,sgr1,sgr2,sgr3)
    val cs2 = Chromosome(ChromosomeType.SEXUAL_X,X,sgr1,sgr2,sgr3)

    val ccs1 = new ChromosomeCoupleImpl with SexualChromosomeCouple {
      type ChromosomeUnit = SexualChromosome
    }
    ccs1.addChromosomeCouple(cs1,cs2)
    assert(ccs1.gender==Female)

    val ccs2 = new ChromosomeCoupleImpl with SexualChromosomeCouple {
      type ChromosomeUnit = SexualChromosome
    }
    val cs3 = Chromosome(ChromosomeType.SEXUAL_Y,Y)
    ccs2.addChromosomeCouple(cs1,cs3)
    assert(ccs2.gender==Male)
  }
}
