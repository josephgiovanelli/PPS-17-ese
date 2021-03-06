package it.unibo.pps.ese.model.genetics.dnaexpression

import it.unibo.pps.ese.model.genetics._
import it.unibo.pps.ese.model.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.model.genetics.dna.{ChromosomeType, GeneType, RegulatorGene, StructuralGene}
import it.unibo.pps.ese.model.genetics.dnaexpression.AllelicBehaviour.getDominant
import it.unibo.pps.ese.model.genetics.entities.QualityType

/**
  * Contains the expression logic for the translation of a gene
  */
sealed trait ExpressionLogic{
  /**
    * To express the behaviour of a couple of alleles
    * @param allConversionMap
    *                         The list of [[ConversionMap]] that affect the qualities
    * @param a1
    *           The first [[AllelicBehaviour]]
    * @param a2
    *           The second [[AllelicBehaviour]]
    * @param animalFeature
    *                      The [[AnimalFeature]] to update
    * @return
    *         The updated  [[AnimalFeature]]
    */
  def expressBehavior(
                       allConversionMap:Seq[ConversionMap],
                       a1:AllelicBehaviour,
                       a2:AllelicBehaviour,
                       animalFeature:AnimalFeature):AnimalFeature
}

/**
  * Companin object of [[ExpressionLogic]] to obtain the right ExpressionLogic
  */
object ExpressionLogic{
  def apply(geneType: GeneType): ExpressionLogic = geneType match {
    case StructuralGene => StructuralGeneExpressionLogic
    case RegulatorGene => RegulatorGeneExpressionLogic
    case _=> throw new IllegalArgumentException
  }

  def apply(chromosomeType: ChromosomeType): ExpressionLogic = chromosomeType match {
    case ChromosomeType.SEXUAL_X => SexualExpressionLogic
  }

  object SexualExpressionLogic extends ExpressionLogic{
    override def expressBehavior(
                                  allConversionMap: Seq[ConversionMap],
                                  a1: AllelicBehaviour,
                                  a2: AllelicBehaviour,
                                  animalFeature: AnimalFeature): AnimalFeature = {
      affectQualityByAllele(a1,animalFeature)
    }
  }

  private[this] def affectQualityByAllele(a:AllelicBehaviour,animalFeature: AnimalFeature):AnimalFeature = {
    animalFeature.addActiveStructuralAllele(a)
    a.featuresBehaviour.foreach(f=>{
      f._1.conversionMaps.foreach(e=>{
        animalFeature.affectQuality(e.qualityAffected,e.effectRatio*f._2)
      })
    })
    animalFeature.affectQuality(QualityType.EnergyRequirements,a.energyConsumption)
    animalFeature
  }

  private [ExpressionLogic] object StructuralGeneExpressionLogic extends ExpressionLogic{
    override def expressBehavior(
                                  allConversionMap: Seq[ConversionMap],
                                  a1: AllelicBehaviour,
                                  a2: AllelicBehaviour,
                                  animalFeature: AnimalFeature): AnimalFeature
    = getDominant(a1,a2) match{
      case Some(a) => affectQualityByAllele(a,animalFeature)
      case None => affectQualityByAllele(Utilities.pickRandomElement(a1,a2),animalFeature)
    }
  }

  private [ExpressionLogic] object RegulatorGeneExpressionLogic extends ExpressionLogic{
    override def expressBehavior(
                                  allConversionMap: Seq[ConversionMap],
                                  a1: AllelicBehaviour,
                                  a2: AllelicBehaviour,
                                  animalFeature: AnimalFeature): AnimalFeature
    = getDominant(a1,a2) match{
      case Some(a) => affectQualityByAllele(a,animalFeature)
      case None => affectQualityByAlleleCouple(a1,a2,animalFeature)
    }

    def affectQualityByAlleleCouple(a1:AllelicBehaviour,a2:AllelicBehaviour,animalFeature: AnimalFeature):AnimalFeature = {
      animalFeature.addActiveStructuralAllele(a1)
      animalFeature.addActiveStructuralAllele(a2)
      a1.featuresBehaviour.foreach(f=>{
        f._1.conversionMaps.foreach(e=>{
          val a2Effect:Double = a2.featuresBehaviour.find(_._1 == f._1).get._2
          animalFeature.affectQuality(e.qualityAffected,e.effectRatio*((f._2+a2Effect)/2))
        })
      })
      animalFeature.affectQuality(QualityType.EnergyRequirements,(a1.energyConsumption+a2.energyConsumption)/2)
      animalFeature
    }
  }

}