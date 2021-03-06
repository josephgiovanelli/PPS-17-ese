package it.unibo.pps.ese.model.genetics

import it.unibo.pps.ese.model.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import org.scalatest.FunSuite
import it.unibo.pps.ese.model.genetics.dna.{AnimalGenome, GeneWithAllelicForms, RegulatorGene}
import it.unibo.pps.ese.model.genetics.dnaexpression._
import it.unibo.pps.ese.model.genetics.entities.QualityType._
import it.unibo.pps.ese.model.genetics.entities.{AnimalInfo, Male}
import it.unibo.pps.ese.model.genetics.generators.SpeciesUtilities
import it.unibo.pps.ese.model.genetics.generators.data.TranslatedAnimalDataImpl
class TestSetupSpecies extends FunSuite{
  test("Test setup"){
    val g1:Seq[ProteinoGenicAmminoacid] = List('A')
    val a1:Seq[ProteinoGenicAmminoacid] = List('C')
    val a2:Seq[ProteinoGenicAmminoacid] = List('D')

    val cmap:ConversionMap = ConversionMap(Speed,1)
    val cmap2:ConversionMap = ConversionMap(Speed,2)
    val feature:Feature = Feature("muscolatura",List(cmap))
    val feature2:Feature = Feature("lunghezza",List(cmap2))
    val featuresBehaviour1:Seq[(Feature,Double)] = List((feature,2.0),(feature2,2.0))
    val featuresBehaviour2:Seq[(Feature,Double)] = List((feature,3.0),(feature2,3.0))
    val AlleleInfo1:AlleleInfo = AlleleInfo(g1,a1,5,featuresBehaviour1,10,0.5)
    val AlleleInfo2:AlleleInfo = AlleleInfo(g1,a2,4,featuresBehaviour2,5,0.5)
    val genedataStruc:GeneData = GeneData(g1,"Gambe",List(feature,feature2),List(AlleleInfo1,AlleleInfo2))

    //LIFE CYCLE CHROMOSOME
    val lcgs1:Seq[ProteinoGenicAmminoacid] = List('T')
    val lcgs2:Seq[ProteinoGenicAmminoacid] = List('U')
    val lcgs3:Seq[ProteinoGenicAmminoacid] = List('V')
    val lcgs4:Seq[ProteinoGenicAmminoacid] = List('W')

    //allelesCode
    val lcgs1a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val lcgs1a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val lcgs2a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val lcgs2a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val lcgs3a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val lcgs3a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val lcgs4a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val lcgs4a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val cmaps1:ConversionMap = ConversionMap(Life,1)
    val features1:Feature = Feature("vitamassima",List(cmaps1))
    val featuresBehaviours11:Seq[(Feature,Double)] = List((features1,2.0))
    val featuresBehaviours12:Seq[(Feature,Double)] = List((features1,3.0))
    val AlleleInfos11:AlleleInfo = AlleleInfo(lcgs1,lcgs1a1,5,featuresBehaviours11,10,0.5)
    val AlleleInfos12:AlleleInfo = AlleleInfo(lcgs1,lcgs1a2,4,featuresBehaviours12,5,0.5)
    val genedatalc1:GeneData = GeneData(lcgs1,"vitaMassima",List(features1),List(AlleleInfos11,AlleleInfos12))

    val cmaps2:ConversionMap = ConversionMap(Childhood,1)
    val features2:Feature = Feature("child",List(cmaps2))
    val featuresBehaviours21:Seq[(Feature,Double)] = List((features2,2.0))
    val featuresBehaviours22:Seq[(Feature,Double)] = List((features2,3.0))
    val AlleleInfos21:AlleleInfo = AlleleInfo(lcgs2,lcgs2a1,5,featuresBehaviours21,10,0.5)
    val AlleleInfos22:AlleleInfo = AlleleInfo(lcgs2,lcgs2a2,4,featuresBehaviours22,5,0.5)
    val genedatalc2:GeneData = GeneData(lcgs2,"child",List(features2),List(AlleleInfos21,AlleleInfos22))

    val cmaps3:ConversionMap = ConversionMap(Maturity,1)
    val features3:Feature = Feature("adult",List(cmaps3))
    val featuresBehaviours31:Seq[(Feature,Double)] = List((features3,2.0))
    val featuresBehaviours32:Seq[(Feature,Double)] = List((features3,3.0))
    val AlleleInfos31:AlleleInfo = AlleleInfo(lcgs3,lcgs3a1,5,featuresBehaviours31,10,0.5)
    val AlleleInfos32:AlleleInfo = AlleleInfo(lcgs3,lcgs3a2,4,featuresBehaviours32,5,0.5)
    val genedatalc3:GeneData = GeneData(lcgs3,"adult",List(features3),List(AlleleInfos31,AlleleInfos32))

    val cmaps4:ConversionMap = ConversionMap(Decline,1)
    val features4:Feature = Feature("decay",List(cmaps4))
    val featuresBehaviours41:Seq[(Feature,Double)] = List((features4,2.0))
    val featuresBehaviours42:Seq[(Feature,Double)] = List((features4,3.0))
    val AlleleInfos41:AlleleInfo = AlleleInfo(lcgs4,lcgs4a1,5,featuresBehaviours41,10,0.5)
    val AlleleInfos42:AlleleInfo = AlleleInfo(lcgs4,lcgs4a2,4,featuresBehaviours42,5,0.5)
    val genedatalc4:GeneData = GeneData(lcgs4,"decay",List(features4),List(AlleleInfos41,AlleleInfos42))

    //SEXUAL GENES
    val cg1:Seq[ProteinoGenicAmminoacid] = List('A','E')
    val cg2:Seq[ProteinoGenicAmminoacid] = List('C','E')
    val as2:Seq[ProteinoGenicAmminoacid] = List('D')

    val gs= GeneWithAllelicForms(cg1,as2,RegulatorGene)
    val gs2 = GeneWithAllelicForms(cg2,as2,RegulatorGene)

    val cmapS:ConversionMap = ConversionMap(Fertility,1)
    val cmapS2:ConversionMap = ConversionMap(Fecundity,1)

    val featureS:Feature = Feature("fecondita",List(cmapS))
    val featureS2:Feature = Feature("fertilita",List(cmapS2))

    val featuresBehaviourS1:Seq[(Feature,Double)] = List((featureS,2.0))
    val featuresBehaviourS2:Seq[(Feature,Double)] = List((featureS2,3.0))

    val AlleleInfoSx1:AlleleInfo = AlleleInfo(cg1,as2,4,featuresBehaviourS1,10,1.0)
    val AlleleInfoSx2:AlleleInfo = AlleleInfo(cg2,as2,3,featuresBehaviourS2,5,1.0)

    val genedataSx1:GeneData = GeneData(cg1,"Fecondita",List(featureS),List(AlleleInfoSx1))
    val genedataSx2:GeneData = GeneData(cg2,"Fertilita",List(featureS2),List(AlleleInfoSx2))


    val speciesSetup:SpeciesUtilities = SpeciesUtilities(TranslatedAnimalDataImpl(
      name = "Cane gatto",
      geneLength = 20,
      reign = "A",
      typology = "C",
      structuralChromosome = List(genedataStruc),
      regulationChromosome = List(genedatalc1,genedatalc2,genedatalc3,genedatalc4,genedatalc4),
      sexualChromosome = List(genedataSx1,genedataSx2,genedataSx2)
    ))
    val animalGenome:AnimalGenome = speciesSetup.generateAnimalGenome
    val animalFeature:AnimalInfo = speciesSetup.translateGenome(animalGenome)
    println(animalFeature)
    if(animalFeature.gender == Male){
      assert(!(animalFeature.qualities contains Fecundity))
    }else{
      assert(animalFeature.qualities contains Fertility)
      assert(animalFeature.qualities contains Fecundity)
      assert(animalFeature.qualities(Fertility).qualityValue==2.0)
      assert(animalFeature.qualities(Fecundity).qualityValue==6.0)
    }
    val speed:Double = animalFeature
      .qualities(Speed)
      .qualityValue
    assert(speed==6.0||speed==9.0)
    val qualityValue:Double = animalFeature
      .qualities(Life)
      .qualityValue
    assert(qualityValue==2.0 || qualityValue == 3.0)
    println(animalGenome)
  }
}
