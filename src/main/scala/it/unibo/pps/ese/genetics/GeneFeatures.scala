package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

sealed trait GeneFeatures{
  def geneSeq:Seq[ProteinoGenicAmminoacid]
  def name:String
  def geneFeatures:Seq[Feature]
  def allelicForm:Seq[AllelicBehaviour]
}

trait Feature {
  def name:String
  def conversionMaps:Seq[ConversionMap]
}

trait ConversionMap{
  def qualityAffected:QualityType
  def effectRatio:Double
}


sealed trait AllelicBehaviour{
  def geneSeq:Seq[ProteinoGenicAmminoacid]
  def allelicSeq:Seq[ProteinoGenicAmminoacid]
  def dominanceLevel:Int
  def featuresBehaviour:Seq[(Feature,Double)]
  def energyConsumption:Double
}


object AllelicBehaviour{
  def apply(
             geneSeq:Seq[ProteinoGenicAmminoacid],
             allelicSeq: Seq[ProteinoGenicAmminoacid],
             dominanceLevel: Int,
             featuresBehaviour: Seq[(Feature, Double)],
             energyConsumption: Double): AllelicBehaviour = AllelicBehaviourImpl(
    geneSeq,
    allelicSeq,
    dominanceLevel,
    featuresBehaviour,
    energyConsumption)
  def getDominant(a1:AllelicBehaviour,a2:AllelicBehaviour):Option[AllelicBehaviour] = {
    val a1d= a1.dominanceLevel
    val a2d = a2.dominanceLevel
    if( a1d==a2d) None else {if(a1d > a2d) Some(a1) else Some(a2)}
  }
  case class AllelicBehaviourImpl(
                                   geneSeq:Seq[ProteinoGenicAmminoacid],
                                   allelicSeq:Seq[ProteinoGenicAmminoacid],
                                   dominanceLevel:Int,
                                   featuresBehaviour:Seq[(Feature,Double)],
                                   energyConsumption:Double
                                 )extends AllelicBehaviour
}


object GeneFeatures{
  def apply(
             geneSeq:Seq[ProteinoGenicAmminoacid],
             name:String,
             geneFeatures:Seq[Feature],
             allelicForm:Seq[AllelicBehaviour]
           ): GeneFeatures = new GeneFeaturesImpl(
    geneSeq,name,geneFeatures,allelicForm
  )
  case class GeneFeaturesImpl(
                               override val geneSeq:Seq[ProteinoGenicAmminoacid],
                               override val name:String,
                               override val geneFeatures:Seq[Feature],
                               override val allelicForm:Seq[AllelicBehaviour]
                             )extends GeneFeatures
}
object Feature{
  def apply(name:String,features:Seq[ConversionMap]): Feature = FeatureImpl(name,features)
  private[this] case class FeatureImpl(override val name:String, override val conversionMaps: Seq[ConversionMap]) extends Feature
}

object ConversionMap {
  def apply(qualityAffected: QualityType, effectRatio: Double): ConversionMap = ConversionMapImpl(qualityAffected, effectRatio)

  private[this] case class ConversionMapImpl(
                                              override val qualityAffected: QualityType,
                                              override val effectRatio: Double)
    extends ConversionMap

}