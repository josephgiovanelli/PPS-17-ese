package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import AmminoAcidUtilities._
import it.unibo.pps.ese.genetics.DnaTranslator.DnaTranslatorImpl

class SpeciesSetup(animalData: AnimalData) {
  val allGeneData:Seq[GeneData] = animalData.structuralChromosome ++
                                  animalData.regulationChromosome ++
                                  animalData.sexualChromosome

  val geneFeatures:Seq[GeneFeatures] =allGeneData.map(geneData=>{
    val allelicBehaviours:Seq[AllelicBehaviour] = geneData
                      .allelicForm
                      .map(ad=>AllelicBehaviour(
                        ad.geneSeq,
                        ad.allelicSeq,
                        ad.dominanceLevel,
                        ad.featuresBehaviour,
                        ad.energyConsumption))
    GeneFeatures(geneData.geneSeq,geneData.name,geneData.geneFeatures,allelicBehaviours)
  })
  val dnaTranslator:DnaTranslator = new DnaTranslatorImpl(geneFeatures)

  val speciesGenerator:SpeciesGenerator = new SpeciesGenerator(
    commonChromosomeGenes = List(stringToReign(animalData.reign),speciesNameToGene(animalData.name)),
    structuralChromosomeGenes = allGenes(animalData.structuralChromosome),
    lifeCycleChromosomeGenes = allGenes(animalData.regulationChromosome),
    feedingChromosomeGenes = List(stringToDiet(animalData.typology)),
    sexualChromosomeGenes = allGenes(animalData.sexualChromosome)
  )

  def stringToReign(s:String):BasicGene = {
    val aS:String = Animal.reignName
    val pS:String = Plant.reignName
    s match {
      case `aS` => Animal.geneId
      case `pS` => Plant.geneId
    }
  }

  def speciesNameToGene(s:String):BasicGene = {
    val speciedCodeSeq:Seq[ProteinoGenicAmminoacid]=
      s.toLowerCase
      .toSeq
      .filter(c=>ProteinoGenicAmminoacid
        .values
        .map(_.shortName)
        .contains(c))
    BasicGene(speciedCodeSeq,IdentifierGene)
  }
  def allGenes(genes:Seq[GeneData]):Seq[GeneWithPossibleAlleles] = {
    genes.map(geneData=>{
      val alleles:Seq[AlleleWithProbability] = geneData
                                                      .allelicForm
                                                      .map(allele=>
                                                        AlleleWithProbability(allele.allelicSeq,allele.probability))
      GeneWithPossibleAlleles(geneData.geneSeq,alleles)
    })
  }

  def stringToDiet(s:String):BasicGene = {
    val eS:String = Herbivore.dietName
    val cS:String = Carnivorous.dietName
    s match {
      case `eS` => Herbivore.geneId
      case `cS` => Carnivorous.geneId
    }
  }

}
