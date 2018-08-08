package it.unibo.pps.ese.genetics

import org.scalatest.FunSuite
import it.unibo.pps.ese.genetics.AmminoAcidUtilities._
import it.unibo.pps.ese.genetics.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.DnaTranslator.DnaTranslatorImpl
import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

class TestDnaTranslation extends FunSuite{

  def buildAnimalGenome(geneType: GeneType,
                        chromosomeType: ChromosomeType,
                        dominanceLevelA1:Int,
                        dominanceLevelA2:Int)
  :(AnimalGenome,DnaTranslator) = {
    val g1:Seq[ProteinoGenicAmminoacid] = List('A')
    val a1:Seq[ProteinoGenicAmminoacid] = List('C')
    val a2:Seq[ProteinoGenicAmminoacid] = List('D')

    val cmap:ConversionMap = ConversionMap(Speed,1)
    val cmap2:ConversionMap = ConversionMap(Speed,2)

    val feature:Feature = Feature("muscolatura",List(cmap))
    val feature2:Feature = Feature("lunghezza",List(cmap2))

    val featuresBehaviour1:Seq[(Feature,Double)] = List((feature,2.0),(feature2,2.0))
    val featuresBehaviour2:Seq[(Feature,Double)] = List((feature,3.0),(feature2,3.0))

    val allelicBehaviour1:AllelicBehaviour = AllelicBehaviour(g1,a1,dominanceLevelA1,featuresBehaviour1,10)
    val allelicBehaviour2:AllelicBehaviour = AllelicBehaviour(g1,a2,dominanceLevelA2,featuresBehaviour2,5)

    val gs1a = GeneWithAllelicForms(List('A'),List('C'),geneType)
    val gs2a = GeneWithAllelicForms(List('A'),List('D'),geneType)

    val cs1a = Chromosome(chromosomeType,gs1a)
    val cs2a = Chromosome(chromosomeType,gs2a)
    val cc2a = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    cc2a.addChromosomeCouple(cs1a,cs2a)
    val cg1:Seq[ProteinoGenicAmminoacid] = List('A','E')
    val cg2:Seq[ProteinoGenicAmminoacid] = List('C','E')
    val as2:Seq[ProteinoGenicAmminoacid] = List('D')

    val gs= GeneWithAllelicForms(cg1,as2,RegulatorGene)
    val gs2 = GeneWithAllelicForms(cg2,as2,RegulatorGene)

    val cs1 = Chromosome(ChromosomeType.SEXUAL_X,X,gs,gs2)
    val cs2 = Chromosome(ChromosomeType.SEXUAL_X,X,gs,gs2)

    val cmapS:ConversionMap = ConversionMap(Fertility,1)
    val cmapS2:ConversionMap = ConversionMap(Fecondity,1)

    val featureS:Feature = Feature("fecondita",List(cmapS))
    val featureS2:Feature = Feature("fertilita",List(cmapS2))

    val ccs2 = new ChromosomeCoupleImpl with SexualChromosomeCouple {
      type ChromosomeUnit = SexualChromosome
    }

    val featuresBehaviourS1:Seq[(Feature,Double)] = List((featureS,2.0))
    val featuresBehaviourS2:Seq[(Feature,Double)] = List((featureS2,3.0))

    val allelicBehaviourS1:AllelicBehaviour = AllelicBehaviour(cg1,as2,dominanceLevelA1,featuresBehaviourS1,10)
    val allelicBehaviourS2:AllelicBehaviour = AllelicBehaviour(cg2,as2,dominanceLevelA2,featuresBehaviourS2,5)

    ccs2.addChromosomeCouple(cs1,cs2)
//    println(ccs2.firstChromosome.geneList)
    val geneFeatures:GeneFeatures = GeneFeatures(g1,"Gambe",List(feature,feature2),List(allelicBehaviour1,allelicBehaviour2))
    val geneFeaturesS1:GeneFeatures = GeneFeatures(cg1,"Fecondita",List(featureS),List(allelicBehaviourS1))
    val geneFeaturesS2:GeneFeatures = GeneFeatures(cg2,"Fertilita",List(featureS2),List(allelicBehaviourS2))

    val dnaTranslator:DnaTranslator = new DnaTranslatorImpl(List(geneFeatures,geneFeaturesS1,geneFeaturesS2))

    (AnimalGenome(Map(
      chromosomeType->cc2a),ccs2),dnaTranslator)
  }

  test("Test the translation of animal " +
    "structural chromosome couple with dominant allele"){

    val couple:(AnimalGenome,DnaTranslator) = buildAnimalGenome(
      StructuralGene,
      ChromosomeType.STRUCTURAL_ANIMAL,
      5,4
    )
    val animalGenome:AnimalGenome = couple._1
    val dnaTranslator:DnaTranslator = couple._2
    val qualityValue:Double = dnaTranslator
                                          .getQualitiesByGenome(animalGenome)
                                          .animalQualities(Speed)
                                          .qualityValue
    assert(qualityValue==6.0)

    println(dnaTranslator.getQualitiesByGenome(animalGenome))
  }


  test("Test the translation of animal " +
    "structural chromosome couple with no dominant allele"){

    val couple:(AnimalGenome,DnaTranslator) = buildAnimalGenome(
      StructuralGene,
      ChromosomeType.STRUCTURAL_ANIMAL,
      4,4
    )
    val animalGenome:AnimalGenome = couple._1
    val dnaTranslator:DnaTranslator = couple._2
    val qualityValue:Double = dnaTranslator
      .getQualitiesByGenome(animalGenome)
      .animalQualities(Speed)
      .qualityValue
    assert(qualityValue==6.0 || qualityValue == 9.0)

    println(dnaTranslator.getQualitiesByGenome(animalGenome))
  }

  test("Test the translation of animal " +
    "regulator chromosome couple with dominant allele"){

    val couple:(AnimalGenome,DnaTranslator) = buildAnimalGenome(
      RegulatorGene,
      ChromosomeType.ONLY_FOR_TEST,
      5,4
    )
    val animalGenome:AnimalGenome = couple._1
    val dnaTranslator:DnaTranslator = couple._2
    val qualityValue:Double = dnaTranslator
      .getQualitiesByGenome(animalGenome)
      .animalQualities(Speed)
      .qualityValue
    assert(qualityValue==6.0)
    println(dnaTranslator.getQualitiesByGenome(animalGenome))
  }

  test("Test the translation of animal " +
    "regulator chromosome couple with no dominant allele"){

    val couple:(AnimalGenome,DnaTranslator) = buildAnimalGenome(
      RegulatorGene,
      ChromosomeType.ONLY_FOR_TEST,
      4,4
    )
    val animalGenome:AnimalGenome = couple._1
    val dnaTranslator:DnaTranslator = couple._2
    val qualityValue:Double = dnaTranslator
      .getQualitiesByGenome(animalGenome)
      .animalQualities(Speed)
      .qualityValue
    assert(qualityValue==7.5)
    println(dnaTranslator.getQualitiesByGenome(animalGenome))
  }
}
