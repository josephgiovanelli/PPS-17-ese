package it.unibo.pps.ese.entitybehaviors.util.reproduction

import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.dna.{AnimalGenome, Chromosome, ChromosomeCouple, ChromosomeType, MGene, SexualChromosomeCouple}
import it.unibo.pps.ese.genetics.entities.AnimalInfo

import scala.util.Random

object EmbryosUtil {

  def createEmbryos(firstReproductionInfo: ReproductionInfo, secondReproductionInfo: ReproductionInfo,
                    fecundity: Double)(implicit geneticEngine: GeneticsEngine): Seq[AnimalInfo] = {
    require(firstReproductionInfo.species == firstReproductionInfo.species)
    var embryosNumber = fecundity.toLong
    if(fecundity - embryosNumber > math.random()) embryosNumber += 1
    (1L to embryosNumber)
      .filter(_ => math.random() > (firstReproductionInfo.fertility + secondReproductionInfo.fertility) / 2)
      .map(_ => makeEmbryo(firstReproductionInfo, secondReproductionInfo))
  }

  def makeEmbryo(firstReproductionInfo: ReproductionInfo, secondReproductionInfo: ReproductionInfo)
                (implicit geneticEngine: GeneticsEngine): AnimalInfo = {
    require(firstReproductionInfo.species == firstReproductionInfo.species)
    val childGenome = Map(
      generateNewChromosomeCouple(firstReproductionInfo.genome.autosomeChromosomeCouples(ChromosomeType.COMMON),
        secondReproductionInfo.genome.autosomeChromosomeCouples(ChromosomeType.COMMON), firstReproductionInfo.species),
      generateNewChromosomeCouple(firstReproductionInfo.genome.autosomeChromosomeCouples(ChromosomeType.STRUCTURAL_ANIMAL),
        secondReproductionInfo.genome.autosomeChromosomeCouples(ChromosomeType.STRUCTURAL_ANIMAL), firstReproductionInfo.species),
      generateNewChromosomeCouple(firstReproductionInfo.genome.autosomeChromosomeCouples(ChromosomeType.LIFE_CYCLE),
        secondReproductionInfo.genome.autosomeChromosomeCouples(ChromosomeType.LIFE_CYCLE), firstReproductionInfo.species),
      generateNewChromosomeCouple(firstReproductionInfo.genome.autosomeChromosomeCouples(ChromosomeType.FEEDING),
        secondReproductionInfo.genome.autosomeChromosomeCouples(ChromosomeType.FEEDING), firstReproductionInfo.species)
    ) |%-%| generateNewSexualChromosomeCouple(firstReproductionInfo.genome.sexualChromosomeCouple,
      secondReproductionInfo.genome.sexualChromosomeCouple, firstReproductionInfo.species)
    geneticEngine.getAnimalInfoByGenome(firstReproductionInfo.species, childGenome)
  }


  def generateNewChromosomeCouple(firstCouple: ChromosomeCouple, secondCouple: ChromosomeCouple, species: String)
                                 (implicit geneticEngine: GeneticsEngine): (ChromosomeType, ChromosomeCouple) = {
    mutateChromosome(randomChromosome(firstCouple), species) |->|
      mutateChromosome(randomChromosome(secondCouple), species)
  }

  def generateNewSexualChromosomeCouple(firstCouple: SexualChromosomeCouple, secondCouple: SexualChromosomeCouple,
                                        species: String)(implicit geneticEngine: GeneticsEngine): SexualChromosomeCouple = {
    mutateChromosome(randomChromosome(firstCouple), species) :+:
      mutateChromosome(randomChromosome(secondCouple), species)
  }

  def mutateChromosome[T<:Chromosome](chromosome: T, species: String)(implicit geneticEngine: GeneticsEngine): T = {
    val newGeneList = chromosome.geneList.map(gene => {
      if(math.random() < geneticEngine.mutationProb && geneticEngine.obtainMutantAlleles(species, gene).nonEmpty)
        Random.shuffle(geneticEngine.obtainMutantAlleles(species, gene)).head
      else
        gene
    })
    chromosome.mutate(newGeneList)
  }

  private def randomChromosome(couple: ChromosomeCouple): couple.ChromosomeUnit = {
    if(Random.nextBoolean()) {
      couple.firstChromosome
    } else {
      couple.secondChromosome
    }
  }

//  implicit class ProbabilisticChromosomeCouple[T <: ChromosomeCouple](couple: T) {
//    def randomChromosome: couple.ChromosomeUnit = {
//      if(Random.nextBoolean()) {
//        couple.firstChromosome
//      } else {
//        couple.secondChromosome
//      }
//    }
//  }

}


case class ReproductionInfo (genome: AnimalGenome, fertility: Double, species: String)