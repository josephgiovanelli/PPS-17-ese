package it.unibo.pps.ese.genetics.generators

import it.unibo.pps.ese.genetics.dna._
import it.unibo.pps.ese.genetics.dnaexpression._
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, QualityType, Species}
import it.unibo.pps.ese.genetics.generators.data.TranslatedAnimalData

@SerialVersionUID(100L)
sealed trait SpeciesUtilities extends Serializable {
  def generateAnimal:AnimalInfo = translateGenome(generateAnimalGenome)
  def generateAnimalGenome:AnimalGenome
  def generateNumberOfAnimal(n:Int):Seq[AnimalInfo]= List.range(0,n).map(_=>generateAnimal)
  def translateGenome(genome:AnimalGenome):AnimalInfo
  def obtainMutantAlleles(gene:MGene):Seq[MGene]
  def checkNewApparitions(genes:Seq[MGene]):Seq[MGene]
  private[genetics] def getAllelicBehaviorOfGene(gene:GeneWithAllelicForms):AllelicBehaviour
  private[genetics] def getFeaturesOfGene(gene:GeneWithAllelicForms):Seq[QualityType]
  private[genetics] def getProbabilityOfGene(gene:GeneWithAllelicForms):Double
}

object SpeciesUtilities{
  def apply(animalData:TranslatedAnimalData):SpeciesUtilities = new SpeciesSetup(animalData)
  private[this] class SpeciesSetup(animalData: TranslatedAnimalData) extends SpeciesUtilities {

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

    val mutantAllele: Seq[AlleleInfo] = allGeneData.flatMap(_.allelicForm).filter(_.probability == 0)

    var notAppearedMutation: Seq[AlleleInfo] = mutantAllele

    def allGenes(genes: Seq[GeneData]): Seq[GeneWithPossibleAlleles] = {
      genes.map(geneData => {
        val alleles: Seq[AlleleWithProbability] = geneData
          .allelicForm
          .filter(_.probability > 0)
          .map(allele =>
            AlleleWithProbability(allele.allelicSeq, allele.probability))
        GeneWithPossibleAlleles(geneData.geneSeq, alleles)
      })
    }

    val geneFeatures: Seq[GeneFeatures] = allGeneData.map(geneData => {
      val allelicBehaviours: Seq[AllelicBehaviour] = geneData
        .allelicForm
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
        .flatMap(_.allelicForm)
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
  }
}

