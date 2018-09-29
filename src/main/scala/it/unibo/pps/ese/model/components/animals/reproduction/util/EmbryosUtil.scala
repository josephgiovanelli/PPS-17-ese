package it.unibo.pps.ese.model.components.animals.reproduction.util

import it.unibo.pps.ese.model.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.model.genetics.dna.{AnimalGenome, Chromosome, ChromosomeCouple, ChromosomeType, SexualChromosomeCouple}
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo

import scala.util.Random

object EmbryosUtil {

  /** Create a quantity of embryos based to given fecundity starting from parents' reproduction information
    *
    * @param firstReproductionInfo ReproductionInfo of first parent
    * @param secondReproductionInfo ReproductionInfo of second parent
    * @param fecundity Fecundity
    * @param geneticEngine Required GeneticEngine
    * @return
    */
  def createEmbryos(firstReproductionInfo: ReproductionInfo, secondReproductionInfo: ReproductionInfo,
                    fecundity: Double)(implicit geneticEngine: GeneticsEngine): Seq[AnimalInfo] = {
    require(firstReproductionInfo.species == firstReproductionInfo.species)
    var embryosNumber = fecundity.toLong
    if(fecundity - embryosNumber > math.random()) embryosNumber += 1
    (1L to embryosNumber)
      .filter(_ => math.random() < (firstReproductionInfo.fertility + secondReproductionInfo.fertility) / 2)
      .map(_ => makeEmbryo(firstReproductionInfo, secondReproductionInfo))
  }

  /** Create an embryos starting from parents' reproduction information
    *
    * @param firstReproductionInfo ReproductionInfo of first parent
    * @param secondReproductionInfo ReproductionInfo of second parent
    * @param geneticEngine Required GeneticEngine
    * @return
    */
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
    ) |%-%| generateNewChromosomeCouple(firstReproductionInfo.genome.sexualChromosomeCouple,
      secondReproductionInfo.genome.sexualChromosomeCouple, firstReproductionInfo.species)
    geneticEngine.getAnimalInfoByGenome(firstReproductionInfo.species, childGenome)
  }


  private[util] def generateNewChromosomeCouple(firstCouple: ChromosomeCouple, secondCouple: ChromosomeCouple, species: String)
                                 (implicit geneticEngine: GeneticsEngine): (ChromosomeType, ChromosomeCouple) = {
    mutateChromosome(randomChromosome(firstCouple), species) |->|
      mutateChromosome(randomChromosome(secondCouple), species)
  }

  private[util] def generateNewChromosomeCouple(firstCouple: SexualChromosomeCouple, secondCouple: SexualChromosomeCouple,
                                        species: String)(implicit geneticEngine: GeneticsEngine): SexualChromosomeCouple = {
    mutateChromosome(randomChromosome(firstCouple), species) :+:
      mutateChromosome(randomChromosome(secondCouple), species)
  }

  private def mutateChromosome[T<:Chromosome](chromosome: T, species: String)(implicit geneticEngine: GeneticsEngine): T = {
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
}

/** Trait that defines an animal reproduction information necessary to simulate reproduction*/
trait ReproductionInfo {
  /** Animal's genome*/
  val genome: AnimalGenome
  /** Animal's fertility*/
  val fertility: Double
  /** Animal's species*/
  val species: String
}

/** Factory object for [[it.unibo.pps.ese.model.components.animals.reproduction.util.ReproductionInfo]]*/
object ReproductionInfo {

  /** Create a [[it.unibo.pps.ese.model.components.animals.reproduction.util.ReproductionInfo]]
    *
    * @param genome Animal's genome
    * @param fertility Animal's fertility
    * @param species Animal's species
    * @return New [[it.unibo.pps.ese.model.components.animals.reproduction.util.ReproductionInfo]]
    */
  def apply(genome: AnimalGenome, fertility: Double, species: String): ReproductionInfo = ReproductionInfoImpl(genome, fertility, species)

  private case class ReproductionInfoImpl(genome: AnimalGenome, fertility: Double, species: String) extends ReproductionInfo
}