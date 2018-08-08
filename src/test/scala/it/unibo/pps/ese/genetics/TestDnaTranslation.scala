package it.unibo.pps.ese.genetics

import org.scalatest.FunSuite
import it.unibo.pps.ese.genetics.AmminoAcidUtilities._
import it.unibo.pps.ese.genetics.DnaTranslater.DnaTranslaterImpl
import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

class TestDnaTranslation extends FunSuite{

  test("Test the translation of animal " +
    "structural chromosome couple with dominant allele"){

    val g1:Seq[ProteinoGenicAmminoacid] = List('A')
    val a1:Seq[ProteinoGenicAmminoacid] = List('C')
    val a2:Seq[ProteinoGenicAmminoacid] = List('D')

    val cmap:ConversionMap = ConversionMap(Speed,1)
    val cmap2:ConversionMap = ConversionMap(Speed,2)

    val feature:Feature = Feature("muscolatura",List(cmap))
    val feature2:Feature = Feature("lunghezza",List(cmap2))

    val featuresBehaviour1:Seq[(Feature,Double)] = List((feature,2.0),(feature2,2.0))
    val featuresBehaviour2:Seq[(Feature,Double)] = List((feature,3.0),(feature2,3.0))

    val allelicBehaviour1:AllelicBehaviour = AllelicBehaviour(g1,a1,5,featuresBehaviour1,10)
    val allelicBehaviour2:AllelicBehaviour = AllelicBehaviour(g1,a2,4,featuresBehaviour2,5)
    val geneFeatures:GeneFeatures = GeneFeatures(g1,"Gambe",List(feature,feature2),List(allelicBehaviour1,allelicBehaviour2))
    val dnaTranslater:DnaTranslater = new DnaTranslaterImpl(List(geneFeatures))

    val gs1a = GeneWithAllelicForms(List('A'),List('C'),StructuralGene)
    val gs2a = GeneWithAllelicForms(List('A'),List('D'),StructuralGene)

    val cs1a = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL,gs1a)
    val cs2a = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL,gs2a)
    val cc2a = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    cc2a.addChromosomeCouple(cs1a,cs2a)

    val gs= GeneWithAllelicForms(List('A'),List('A'),RegulatorGene)
    val gs2 = GeneWithAllelicForms(List('A'),List('F'),RegulatorGene)

    val cs1 = Chromosome(ChromosomeType.SEXUAL_X,X,gs,gs2)
    val cs2 = Chromosome(ChromosomeType.SEXUAL_X,X,gs,gs2)
    val ccs2 = new ChromosomeCoupleImpl with SexualChromosomeCouple {
      type ChromosomeUnit = SexualChromosome
    }
    ccs2.addChromosomeCouple(cs1,cs2)
    val animalGenome:AnimalGenome = AnimalGenome(Map(ChromosomeType.STRUCTURAL_ANIMAL->cc2a),ccs2)
    val qualityValue:Double = dnaTranslater
                                          .getQualitiesByGenome(animalGenome)
                                          .animalQualities(Speed)
                                          .qualityValue
    assert(qualityValue==6.0)

    println(dnaTranslater.getQualitiesByGenome(animalGenome))
  }
}
