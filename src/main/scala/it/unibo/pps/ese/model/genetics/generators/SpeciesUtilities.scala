package it.unibo.pps.ese.model.genetics.generators

import it.unibo.pps.ese.model.genetics.dna._
import it.unibo.pps.ese.model.genetics.dnaexpression._
import it.unibo.pps.ese.model.genetics.entities.{AnimalInfo, QualityType, Species}
import it.unibo.pps.ese.model.genetics.generators.data.TranslatedAnimalData

/**
  * Utilities for a species of animal
  */
sealed trait SpeciesUtilities{
  /**
    * @return a generated [[AnimalInfo]] of a new animal
    */
  def generateAnimal:AnimalInfo = translateGenome(generateAnimalGenome)

  /**
    *
    * @return a generated Genome
    */
  def generateAnimalGenome:AnimalGenome

  /**
    * Create a certain number of animal
    * @param n
    * @return
    */
  def generateNumberOfAnimal(n:Int):Seq[AnimalInfo]= List.range(0,n).map(_=>generateAnimal)

  /**
    * Translate the genome of an Animal
    * @param genome
    * @return
    */
  def translateGenome(genome:AnimalGenome):AnimalInfo

  /**
    * To obtain alle the possible mutan allels
    * @param gene
    * @return
    */
  def obtainMutantAlleles(gene:MGene):Seq[MGene]

  /**
    * Check if the just appeared genes are new mutant alleles
    * @param genes
    * @return
    */
  def checkNewApparitions(genes:Seq[MGene]):Seq[MGene]
  def obtainNotAppearedMutation:Seq[AlleleInfo]
  private[genetics] def restoreOldNotAppearedAlleles(oldNotAppearedAlleles:Seq[AlleleInfo]):Unit
  private[genetics] def getAllelicBehaviorOfGene(gene:GeneWithAllelicForms):AllelicBehaviour
  private[genetics] def getFeaturesOfGene(gene:GeneWithAllelicForms):Seq[QualityType]
  private[genetics] def getProbabilityOfGene(gene:GeneWithAllelicForms):Double
}

object SpeciesUtilities{
  def apply(animalData:TranslatedAnimalData):SpeciesUtilities = new SpeciesSetup(animalData)
  private[this] class SpeciesSetup(animalData: TranslatedAnimalData) extends SpeciesUtilities {
    import SpeciesValues._
    var restored: Boolean = false
    val geneFeatures: Seq[GeneFeatures] = allGeneData.map(geneData => {
      val allelicBehaviours: Seq[AllelicBehaviour] = geneData
        .allelicFormWithProbability
        .map(ad => AllelicBehaviour(
          ad.geneSeq,
          ad.allelicSeq,
          ad.dominanceLevel,
          ad.featuresBehaviour,
          ad.energyConsumption))
      GeneFeatures(geneData.geneSeq, geneData.name, geneData.geneFeatures, allelicBehaviours)
    })

    val dnaTranslator: DnaTranslator = DnaTranslator(geneFeatures)
    override def generateAnimalGenome: AnimalGenome = speciesGenerator.generateAnimalGenome

    override def translateGenome(genome:AnimalGenome): AnimalInfo = AnimalInfo(
      Species(animalData.reign,animalData.name),
      dnaTranslator.getQualitiesByGenome(genome),
      genome
    )

    override def obtainMutantAlleles(gene:MGene):Seq[MGene] ={
      mutantAllele
        .filter(a=>a.geneSeq==gene.geneId)
        .map(a=>GeneWithAllelicForms(a.geneSeq,a.allelicSeq,gene.geneType))
    }

    override def getAllelicBehaviorOfGene(gene: GeneWithAllelicForms): AllelicBehaviour = {
      geneFeatures
        .find(_.geneSeq == gene.geneId)
        .getOrElse(throw new IllegalStateException())
        .allelicForm
        .find(_.allelicSeq == gene.alleleCode)
        .getOrElse(throw new IllegalStateException())
    }

    override private[genetics] def getProbabilityOfGene(gene: GeneWithAllelicForms):Double = {
      allGeneData
        .flatMap(_.allelicFormWithProbability)
        .find(a=>a.geneSeq==gene.geneId && a.allelicSeq==gene.alleleCode)
        .getOrElse(throw new IllegalStateException())
        .probability
    }
    override private[genetics] def getFeaturesOfGene(gene: GeneWithAllelicForms):Seq[QualityType] = {
      allGeneData
        .find(_.geneSeq == gene.geneId)
        .getOrElse(throw new IllegalStateException())
        .geneFeatures
        .flatMap(_.conversionMaps)
        .map(_.qualityAffected)
    }


    private[this] object SpeciesValues {
      val allGeneData: Seq[GeneData] = animalData.structuralChromosome ++
        animalData.regulationChromosome ++
        animalData.sexualChromosome




      val speciesGenerator: SpeciesGenerator = new SpeciesGenerator(
        commonChromosomeGenes = List(animalData.reign, speciesNameToGene(animalData.name)),
        structuralChromosomeGenes = allGenes(animalData.structuralChromosome),
        lifeCycleChromosomeGenes = allGenes(animalData.regulationChromosome),
        feedingChromosomeGenes = List(stringToDiet(animalData.typology)),
        sexualChromosomeGenes = allGenes(animalData.sexualChromosome)
      )
      val mutantAllele: Seq[AlleleInfo] = allGeneData.flatMap(_.allelicFormWithProbability).filter(_.probability == 0)
      var notAppearedMutation: Seq[AlleleInfo] = mutantAllele
      def allGenes(genes: Seq[GeneData]): Seq[GeneWithPossibleAlleles] = {
        genes.map(geneData => {
          val alleles: Seq[AlleleWithProbability] = geneData
            .allelicFormWithProbability
            .filter(_.probability > 0)
            .map(allele =>
              AlleleWithProbability(allele.allelicSeq, allele.probability))
          GeneWithPossibleAlleles(geneData.geneSeq, alleles)
        })
      }
    }

    override def checkNewApparitions(genes: Seq[MGene]): Seq[MGene] = {
      genes.filter{
        case GeneWithAllelicForms(gi,ai,gt) =>
          if(notAppearedMutation.exists(a=> a.geneSeq==gi && a.allelicSeq==ai)){
            notAppearedMutation = notAppearedMutation.filter(a=> a.geneSeq==gi && a.allelicSeq==gt)
            true
          }else{
            false
          }
        case _=> false
      }
    }

    override def obtainNotAppearedMutation: Seq[AlleleInfo] = notAppearedMutation

    override private[genetics] def restoreOldNotAppearedAlleles(oldNotAppearedAlleles: Seq[AlleleInfo]): Unit =
      if(restored){
        throw new IllegalStateException()
      }else{
        notAppearedMutation = oldNotAppearedAlleles
        restored = true
      }
  }
}

