package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ChromosomeType.ChromosomeType
import AllelicBehaviour.getDominant

import scala.util.Random

sealed trait DnaTranslater {
  def getQualitiesByGenome(animalGenome: AnimalGenome):AnimalFeature
}
sealed trait ExpressionLogic{
  def expressBehavior(
                       allConversionMap:Seq[ConversionMap],
                       a1:AllelicBehaviour,
                       a2:AllelicBehaviour,
                       animalFeature:AnimalFeature):AnimalFeature
}
object ExpressionLogic{
  def apply(geneType: GeneType): ExpressionLogic = geneType match {
    case StructuralGene => StructuralGeneExpressionLogic
    case RegulatorGene => RegulatorGeneExpressionLogic
  }

  def affectQualityByAllele(a:AllelicBehaviour,animalFeature: AnimalFeature):AnimalFeature = {
    a.featuresBehaviour.foreach(f=>{
      f._1.conversionMaps.foreach(e=>{
        animalFeature.affectQuality(e.qualityAffected,e.effectRatio*f._2)
        animalFeature.affectQuality(EnergyRequirements,a.energyConsumption)
      })
    })
    animalFeature
  }

  object StructuralGeneExpressionLogic extends ExpressionLogic{
    override def expressBehavior(
                                  allConversionMap: Seq[ConversionMap],
                                  a1: AllelicBehaviour,
                                  a2: AllelicBehaviour,
                                  animalFeature: AnimalFeature): AnimalFeature
    = getDominant(a1,a2) match{
      case Some(a) => {
        affectQualityByAllele(a,animalFeature)
      }
      case None => affectQualityByAllele(pickRandomAllele(a1,a2),animalFeature)
    }
  }
  def pickRandomAllele(a1: AllelicBehaviour,a2: AllelicBehaviour):AllelicBehaviour = Random.nextInt(2) match {
    case 0 => a1
    case 1 => a2
  }

  object RegulatorGeneExpressionLogic extends ExpressionLogic{
    override def expressBehavior(
                                  allConversionMap: Seq[ConversionMap],
                                  a1: AllelicBehaviour,
                                  a2: AllelicBehaviour,
                                  animalFeature: AnimalFeature): AnimalFeature
    = getDominant(a1,a2) match{
      case Some(a) => {
        affectQualityByAllele(a,animalFeature)
      }
      case None => {
        affectQualityByAlleleCouple(a1,a2,animalFeature)
      }

    }

    def affectQualityByAlleleCouple(a1:AllelicBehaviour,a2:AllelicBehaviour,animalFeature: AnimalFeature):AnimalFeature = {
      a1.featuresBehaviour.foreach(f=>{
        f._1.conversionMaps.foreach(e=>{
          val a2Effect:Double = a2.featuresBehaviour.find(_._1 == f._1).get._2
          animalFeature.affectQuality(e.qualityAffected,e.effectRatio*((f._2+a2Effect)/2))
        })
      })
      animalFeature.affectQuality(EnergyRequirements,(a1.energyConsumption+a2.energyConsumption)/2)
      animalFeature
    }
  }

}

object DnaTranslater{
  class DnaTranslaterImpl(val speciesGeneBehaviour:Seq[GeneFeatures] ) extends DnaTranslater{

    override def getQualitiesByGenome(animalGenome: AnimalGenome): AnimalFeature = {

      val gs1:Map[ChromosomeType,Chromosome] = animalGenome.firstGenomeSequence
      val gs2:Map[ChromosomeType,Chromosome] = animalGenome.secondGenomeSequence
      var animalFeature:AnimalFeature = new AnimalFeatureImpl
      val gl1:Seq[Gene] = gs1.flatMap(_._2.geneList).filter(_.geneType!= IdentifierGene).toSeq
      iterateGeneList(gl1,animalFeature)

      def iterateGeneList(gl:Seq[Gene],af: AnimalFeature):AnimalFeature= gl match {
        case (h::t) => {
          val allConversionMap: Seq[ConversionMap] = conversionMapsFromGene(h)
          val alleleCouple:(AllelicBehaviour,AllelicBehaviour) = alleleBehaviourCouple(h,animalGenome)
          ExpressionLogic(h.geneType).expressBehavior(
                                                        allConversionMap,
                                                        alleleCouple._1,
                                                        alleleCouple._2,
                                                        animalFeature
                                                      )
          iterateGeneList(t,af)
        }
        case _=> af
      }

      def conversionMapsFromGene(gene: Gene):Seq[ConversionMap] = {
        findGeneFeatures(gene).flatMap(_.conversionMaps)
      }
      def alleleBehaviourCouple(gene: Gene,animalGenome: AnimalGenome):(AllelicBehaviour,AllelicBehaviour) = {
        val ab1:AllelicBehaviour = findAlleleBehaviour(gene)
        val ab2:AllelicBehaviour = findAlleleBehaviour(findRespectiveGene(gene,animalGenome))
        (ab1,ab2)
      }
      def findRespectiveGene(gene: Gene,animalGenome: AnimalGenome):Gene={
        findGeneOnSequence(gene,animalGenome.secondGenomeSequence)
      }
      def findGeneFeatures(gene: Gene):Seq[Feature] = gene match {
        case GeneWithAllelicForms(gc,ac,gt) => speciesGeneBehaviour
          .find(_.geneSeq==gc)
          .get
          .geneFeatures
      }

      def findAlleleBehaviour(gene: Gene):AllelicBehaviour = gene match {
        case GeneWithAllelicForms(gc,ac,gt) => {
          speciesGeneBehaviour
            .find(_.geneSeq==gc)
            .get
            .allelicForm
            .find(_.allelicSeq == ac)
            .get
        }
      }
      def findGeneOnSequence(gene: Gene,gs:Map[ChromosomeType,Chromosome]):Gene = {
        val gl:Seq[Gene] = gs.flatMap(_._2.geneList).filter(_.geneType!= IdentifierGene).toSeq
        gl.find(_.geneCode == gene.geneCode).get
      }

      animalFeature
    }
    def findAlleleCouple(gene: Gene):Unit = gene match {
      case GeneWithAllelicForms(gc,ac,gt) => {
        speciesGeneBehaviour.find(_.geneSeq==gc).get.allelicForm.find(_.allelicSeq==ac).get
      }
      case _=>
    }
  }

}


