package it.unibo.pps.ese.entitybehaviors.util.reproduction

import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genetics.dna.{AnimalGenome, MGene}
import it.unibo.pps.ese.genetics.entities.AnimalInfo

@SerialVersionUID(100L)
trait GeneticsEngine extends Serializable {
  def getAnimalInfoByGenome(species: String, childGenome: AnimalGenome): AnimalInfo
  def obtainMutantAlleles(species: String, gene: MGene): Iterable[MGene]
  def mutationProb: Double
}

object GeneticsEngine {
  def apply(geneticsSimulator: GeneticsSimulator, mutationProb: Double): GeneticsEngine =
    new GeneticsEngineImpl(geneticsSimulator, mutationProb)

  //TODO clever way?
  private[this] class GeneticsEngineImpl(private val geneticsSimulator: GeneticsSimulator, override val mutationProb: Double) extends GeneticsEngine {
    def getAnimalInfoByGenome(species: String, childGenome: AnimalGenome): AnimalInfo = geneticsSimulator.getAnimalInfoByGenome(species, childGenome)
    def obtainMutantAlleles(species: String, gene: MGene): Iterable[MGene] = geneticsSimulator.obtainMutantAlleles(species, gene)
  }

}
