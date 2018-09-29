package it.unibo.pps.ese.model.components.animals.reproduction.util

import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.dna.{AnimalGenome, MGene}
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo

/** Trait that defines genetics related services and information, not related to animal's personal information,
  * necessary to reproduction
  */
trait GeneticsEngine {
  /**
    * To obtain the [[AnimalInfo]] given the species and the genome of the animal.
    * It translate the dna in information about the animal
    * @param species
    *               The species of the given animal
    * @param childGenome
    *               The [[AnimalGenome]] of the animal
    * @return
    */
  def getAnimalInfoByGenome(species: String, childGenome: AnimalGenome): AnimalInfo

  /**
    * To obtain all the mutant alleles of a given species
    *
    * @param species
    * The string that identify the animal species
    * @param gene
    *     The [[MGene]] for which you want to get the mutant alleles
    * @return
    *         The list of mutant forms
    */
  def obtainMutantAlleles(species: String, gene: MGene): Iterable[MGene]

  /** Probability of gene mutation*/
  val mutationProb: Double
}

/** Factory object for [[it.unibo.pps.ese.model.components.animals.reproduction.util.GeneticsEngine]]*/
object GeneticsEngine {
  def apply(geneticsSimulator: GeneticsSimulator, mutationProb: Double): GeneticsEngine =
    new GeneticsEngineImpl(geneticsSimulator, mutationProb)

  private[this] class GeneticsEngineImpl(private val geneticsSimulator: GeneticsSimulator, override val mutationProb: Double) extends GeneticsEngine {
    def getAnimalInfoByGenome(species: String, childGenome: AnimalGenome): AnimalInfo = geneticsSimulator.getAnimalInfoByGenome(species, childGenome)
    def obtainMutantAlleles(species: String, gene: MGene): Iterable[MGene] = geneticsSimulator.obtainMutantAlleles(species, gene)
  }

}
