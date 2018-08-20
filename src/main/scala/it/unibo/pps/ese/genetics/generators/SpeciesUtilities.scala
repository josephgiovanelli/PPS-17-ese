package it.unibo.pps.ese.genetics.generators

import it.unibo.pps.ese.genetics.Utilities.seqOfElement
import it.unibo.pps.ese.genetics.dna._
import it.unibo.pps.ese.genetics.dnaexpression._
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, Species}
import it.unibo.pps.ese.genetics.generators.data.TranslatedAnimalData

sealed trait SpeciesUtilities{
  def generateAnimal:AnimalInfo = translateGenome(generateAnimalGenome)
  def generateAnimalGenome:AnimalGenome
  def generateNumberOfAnimal(n:Int):Seq[AnimalInfo]= seqOfElement(n,generateAnimal)
  def translateGenome(genome:AnimalGenome):AnimalInfo
  def obtainMutantAlleles(gene:MGene):Seq[MGene]
}

object SpeciesUtilities{
  def apply(animalData:TranslatedAnimalData):SpeciesUtilities = new SpeciesSetup(animalData)
  private[this] class SpeciesSetup(animalData: TranslatedAnimalData) extends SpeciesUtilities {

    import SpeciesValues._
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

    private[this] object SpeciesValues {
      val allGeneData: Seq[GeneData] = animalData.structuralChromosome ++
        animalData.regulationChromosome ++
        animalData.sexualChromosome

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

      val speciesGenerator: SpeciesGenerator = new SpeciesGenerator(
        commonChromosomeGenes = List(animalData.reign, speciesNameToGene(animalData.name)),
        structuralChromosomeGenes = allGenes(animalData.structuralChromosome),
        lifeCycleChromosomeGenes = allGenes(animalData.regulationChromosome),
        feedingChromosomeGenes = List(stringToDiet(animalData.typology)),
        sexualChromosomeGenes = allGenes(animalData.sexualChromosome)
      )
      val mutantAllele: Seq[AlleleInfo] = allGeneData.flatMap(_.allelicForm).filter(_.probability == 0)

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
    }
  }
}

