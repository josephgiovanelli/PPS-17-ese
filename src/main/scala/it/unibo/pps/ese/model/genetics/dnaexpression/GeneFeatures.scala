package it.unibo.pps.ese.model.genetics.dnaexpression

import it.unibo.pps.ese.model.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.model.genetics.entities.QualityType

sealed trait GeneInfo{
  /**
    *
    * @return the [[ProteinoGenicAmminoacid]] that identify the gene
    */
  def geneSeq:Seq[ProteinoGenicAmminoacid]

  /**
    *
    * @return the name of the gene
    */
  def name:String

  /**
    *
    * @return A sequence of [[Feature]] that affect some [[it.unibo.pps.ese.model.genetics.entities.Quality]]
    */
  def geneFeatures:Seq[Feature]
}
/**
  * Contains the sequence of [[ProteinoGenicAmminoacid]] that identify the gene,
  * the name of the gene,
  * the seq of [[Feature]] and the [[AllelicBehaviour]] that can be exist
  */
sealed trait GeneFeatures extends GeneInfo{
  /**
    *
    * @return the list [[AllelicBehaviour]] that can exist
    */
  def allelicForm:Seq[AllelicBehaviour]
}

sealed trait GeneData extends GeneInfo{
  /**
    *
    * @return the [[AlleleInfo]] of the Gene
    */
  def allelicFormWithProbability:Seq[AlleleInfo]
}

/**
  * Every features has a name and a conversion map
  */
trait Feature {
  def name:String
  def conversionMaps:Seq[ConversionMap]
}

/**
  * Indicate the affect quality and the ratio of effect
  */
trait ConversionMap{
  def qualityAffected:QualityType
  def effectRatio:Double
}

/**
  * Contains the behaviour of the Allele in Dna Translation
  */
sealed trait AllelicBehaviour{
  def geneSeq:Seq[ProteinoGenicAmminoacid]
  def allelicSeq:Seq[ProteinoGenicAmminoacid]
  def dominanceLevel:Int
  def featuresBehaviour:Seq[(Feature,Double)]
  def energyConsumption:Double
}
sealed trait AlleleInfo extends AllelicBehaviour{
  def probability:Double
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
}

case class AllelicBehaviourImpl(
                                 geneSeq:Seq[ProteinoGenicAmminoacid],
                                 allelicSeq:Seq[ProteinoGenicAmminoacid],
                                 dominanceLevel:Int,
                                 featuresBehaviour:Seq[(Feature,Double)],
                                 energyConsumption:Double
                               )extends AllelicBehaviour

object AlleleInfo {
  def apply(
             geneSeq: Seq[ProteinoGenicAmminoacid],
             allelicSeq: Seq[ProteinoGenicAmminoacid],
             dominanceLevel: Int,
             featuresBehaviour: Seq[(Feature, Double)],
             energyConsumption: Double,
             probability:Double): AlleleInfo =
    new AllelicInfoImpl(
    geneSeq,
    allelicSeq,
    dominanceLevel,
    featuresBehaviour,
    energyConsumption,
    probability)

  class AllelicInfoImpl(
                         geneSeq: Seq[ProteinoGenicAmminoacid],
                         allelicSeq: Seq[ProteinoGenicAmminoacid],
                         dominanceLevel: Int,
                         featuresBehaviour: Seq[(Feature, Double)],
                         energyConsumption: Double,
                         override val probability:Double
                       ) extends AllelicBehaviourImpl(
    geneSeq,
    allelicSeq,
    dominanceLevel,
    featuresBehaviour,
    energyConsumption) with AlleleInfo
}

object GeneFeatures{
  def apply(
             geneSeq:Seq[ProteinoGenicAmminoacid],
             name:String,
             geneFeatures:Seq[Feature],
             allelicForm:Seq[AllelicBehaviour]
           ): GeneFeatures = GeneFeaturesImpl(
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

object GeneData{
  def apply(
             geneSeq:Seq[ProteinoGenicAmminoacid],
             name:String,
             geneFeatures:Seq[Feature],
             allelicForm:Seq[AlleleInfo]
           ): GeneData = GeneDataImpl(
    geneSeq,name,geneFeatures,allelicForm
  )
  case class GeneDataImpl(
                           override val geneSeq:Seq[ProteinoGenicAmminoacid],
                           override val name:String,
                           override val geneFeatures:Seq[Feature],
                           override val allelicFormWithProbability:Seq[AlleleInfo]
                         )extends GeneData
}

