package it.unibo.pps.ese.genetics.dnaexpression

import it.unibo.pps.ese.genetics._
import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.genetics.dna._
import it.unibo.pps.ese.genetics.entities.QualityType.Fertility
import it.unibo.pps.ese.genetics.entities._

@SerialVersionUID(100L)
sealed trait DnaTranslator extends Serializable {
  def getQualitiesByGenome(animalGenome: AnimalGenome):AnimalFeature
}

object DnaTranslator{
  def apply(speciesGeneBehaviour:Seq[GeneFeatures] ): DnaTranslator = new DnaTranslatorImpl(speciesGeneBehaviour)

  private[this]class DnaTranslatorImpl(val speciesGeneBehaviour:Seq[GeneFeatures] ) extends DnaTranslator{

    @SerialVersionUID(100L)
    private[this] object IdentifierGeneTranslator extends Serializable {

      def translateIdentifierGene(gene: MGene, af: AnimalFeature):AnimalFeature = {
        val herbivoreSeq:Seq[ProteinoGenicAmminoacid] = Herbivore.geneId.completeCode
        val carnivorousSeq:Seq[ProteinoGenicAmminoacid] = Carnivorous.geneId.completeCode
        gene.completeCode match {
          case `herbivoreSeq` => af.dietType_(Herbivore);af
          case `carnivorousSeq` => af.dietType_(Carnivorous); af
          case _ => af
        }
      }
    }

    override def getQualitiesByGenome(animalGenome: AnimalGenome): AnimalFeature = {
      import TranslationUtilities._
      val gs1:Map[ChromosomeType,Chromosome] = animalGenome.firstGenomeSequence
      val gs2:Map[ChromosomeType,Chromosome] = animalGenome.secondGenomeSequence
      val animalFeature:AnimalFeature = new AnimalFeatureImpl
      val gl1:Seq[MGene] = gs1.flatMap(_._2.geneList).filter(_.geneType!= IdentifierGene).toSeq
      val gi1:Seq[MGene] = gs1.flatMap(_._2.geneList).filter(_.geneType== IdentifierGene).toSeq
      iterateIdentifierGene(gi1,animalFeature)
      iterateGeneList(gl1,animalFeature,animalGenome)
      translateSexualChromosomeCouple(animalGenome.sexualChromosomeCouple,animalFeature)
      animalFeature
    }

    @SerialVersionUID(100L)
    private[this] object TranslationUtilities extends Serializable {

      def iterateIdentifierGene(gc1:Seq[MGene], animalFeature: AnimalFeature): AnimalFeature
      = gc1 match {
        case h+:t  =>
          iterateIdentifierGene(t,IdentifierGeneTranslator.translateIdentifierGene(h,animalFeature))
        case _ => animalFeature
      }
      def translateSexualChromosomeCouple(scc: SexualChromosomeCouple,af: AnimalFeature):AnimalFeature
      = scc.gender match {
        case Female=>
          af.gender_(Female)
          val chosenChromosome = Utilities.pickRandomElement(scc.firstChromosome,scc.secondChromosome)
          iterateSexualGeneList(chosenChromosome.geneList,af)

        case Male =>
          af.gender_(Male)
          val chosenChromosome = if(scc.firstChromosome.sexualChromosome==X) scc.firstChromosome else scc.secondChromosome
          iterateSexualGeneList(chosenChromosome.geneList,af)
      }

      def iterateSexualGeneList(gList:Seq[MGene], af:AnimalFeature):AnimalFeature ={
        gList.foreach(h=>{
          val allConversionMap: Seq[ConversionMap] = conversionMapsFromGene(h)
          val filter:QualityType=>Boolean = af.gender match {
            case Male=> QualityType.maleSexualQualities.contains(_)
            case Female=> QualityType.femaleSexualQualities.contains(_)
          }
          if(allConversionMap.map(_.qualityAffected).exists(filter)){
            val ab:AllelicBehaviour=findAlleleBehaviour(h)
            ExpressionLogic(ChromosomeType.SEXUAL_X).expressBehavior(
              allConversionMap,
              ab,
              ab,
              af
            )
          }
        })
        af
      }

      def iterateGeneList(gl:Seq[MGene], af: AnimalFeature,animalGenome: AnimalGenome):AnimalFeature= gl match {
        case h+:t =>
          val allConversionMap: Seq[ConversionMap] = conversionMapsFromGene(h)
          val alleleCouple:(AllelicBehaviour,AllelicBehaviour) = alleleBehaviourCouple(h,animalGenome)
          ExpressionLogic(h.geneType).expressBehavior(
            allConversionMap,
            alleleCouple._1,
            alleleCouple._2,
            af
          )
          iterateGeneList(t,af,animalGenome)

        case _=> af
      }

      def conversionMapsFromGene(gene: MGene):Seq[ConversionMap] = {
//        val filter:ConversionMap=>Boolean = gender match {
//          case Some(Female) =>(q)=>QualityType.femaleSexualQualities.contains(q.qualityAffected)
//          case Some(Male) => (q)=>QualityType.maleSexualQualities.contains(q.qualityAffected)
//          case None => (_)=>true
//         }
        findGeneFeatures(gene)
          .flatMap(_.conversionMaps)
//          .filter(filter(_))
      }

      def alleleBehaviourCouple(gene: MGene, animalGenome: AnimalGenome):(AllelicBehaviour,AllelicBehaviour) = {
        val ab1:AllelicBehaviour = findAlleleBehaviour(gene)
        val ab2:AllelicBehaviour = findAlleleBehaviour(findRespectiveGene(gene,animalGenome))
        (ab1,ab2)
      }
      def findRespectiveGene(gene: MGene, animalGenome: AnimalGenome):MGene={
        findGeneOnSequence(gene,animalGenome.secondGenomeSequence)
      }
      def findGeneFeatures(gene: MGene):Seq[Feature] = gene match {
        case GeneWithAllelicForms(gc,ac,gt) => speciesGeneBehaviour
          .find(_.geneSeq==gc)
          .get
          .geneFeatures
      }

      def findAlleleBehaviour(gene: MGene):AllelicBehaviour = gene match {
        case GeneWithAllelicForms(gc,ac,gt) =>
          speciesGeneBehaviour
            .find(_.geneSeq==gc)
            .get
            .allelicForm
            .find(_.allelicSeq == ac)
            .get
      }
      def findGeneOnSequence(gene: MGene, gs:Map[ChromosomeType,Chromosome]):MGene = {
        val gl:Seq[MGene] = gs.flatMap(_._2.geneList).filter(_.geneType!= IdentifierGene).toSeq
        gl.find(_.geneId == gene.geneId).get
      }

    }
  }

}


