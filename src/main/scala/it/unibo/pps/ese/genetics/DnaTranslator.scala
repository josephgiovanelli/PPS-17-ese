package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import AmminoAcidUtilities._
sealed trait DnaTranslator {
  def getQualitiesByGenome(animalGenome: AnimalGenome):AnimalFeature
}

object IdentifierGeneTranslator{

  def translateIdentifierGene(gene: Gene,af: AnimalFeature):AnimalFeature = {
    val herbivoreSeq:Seq[ProteinoGenicAmminoacid] = List('D','E')
    val carnivorousSeq:Seq[ProteinoGenicAmminoacid] = List('D','C')
    gene.completeCode match {
      case `herbivoreSeq` => af.dietType_(Herbivore);af
      case `carnivorousSeq` => af.dietType_(Carnivorous); af
      case _ => af
    }
  }
}

object DnaTranslator{
  class DnaTranslatorImpl(val speciesGeneBehaviour:Seq[GeneFeatures] ) extends DnaTranslator{

    override def getQualitiesByGenome(animalGenome: AnimalGenome): AnimalFeature = {
//      println(speciesGeneBehaviour)
      val gs1:Map[ChromosomeType,Chromosome] = animalGenome.firstGenomeSequence
      val gs2:Map[ChromosomeType,Chromosome] = animalGenome.secondGenomeSequence
      val animalFeature:AnimalFeature = new AnimalFeatureImpl
      val gl1:Seq[Gene] = gs1.flatMap(_._2.geneList).filter(_.geneType!= IdentifierGene).toSeq
      val gi1:Seq[Gene] = gs1.flatMap(_._2.geneList).filter(_.geneType== IdentifierGene).toSeq
      iterateIdentifierGene(gi1,animalFeature)
      iterateGeneList(gl1,animalFeature)
      translateSexualChromosomeCouple(animalGenome.sexualChromosomeCouple,animalFeature)

      def iterateIdentifierGene(gc1:Seq[Gene],animalFeature: AnimalFeature): AnimalFeature
      = gc1 match {
        case h::t  =>
          iterateIdentifierGene(t,IdentifierGeneTranslator.translateIdentifierGene(h,animalFeature))
        case _ => animalFeature
      }
      def translateSexualChromosomeCouple(scc: SexualChromosomeCouple,af: AnimalFeature):AnimalFeature
      = scc.gender match {
        case Female=>{
          af.gender_(Female)
          val chosenChromosome = Utilities.pickRandomElement(scc.firstChromosome,scc.secondChromosome)
          iterateSexualGeneList(chosenChromosome.geneList,af)
        }
        case Male => af.gender_(Male);af
      }

      def iterateSexualGeneList(gList:Seq[Gene],af:AnimalFeature):AnimalFeature ={
          gList.foreach(h=>{
            val allConversionMap: Seq[ConversionMap] = conversionMapsFromGene(h)
            val ab=findAlleleBehaviour(h)
            ExpressionLogic(ChromosomeType.SEXUAL_X).expressBehavior(
              allConversionMap,
              ab,
              ab,
              af
            )
          })
        af
      }

      def iterateGeneList(gl:Seq[Gene],af: AnimalFeature):AnimalFeature= gl match {
        case h::t => {
          val allConversionMap: Seq[ConversionMap] = conversionMapsFromGene(h)
          val alleleCouple:(AllelicBehaviour,AllelicBehaviour) = alleleBehaviourCouple(h,animalGenome)
          ExpressionLogic(h.geneType).expressBehavior(
                                                        allConversionMap,
                                                        alleleCouple._1,
                                                        alleleCouple._2,
                                                        af
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
  }

}


