package it.unibo.pps.ese.genetics.dna

import it.unibo.pps.ese.controller.saving.Memento
import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType

sealed trait Genome{
  def requiredChromosomes:Seq[ChromosomeType]
  def autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple]
  def firstGenomeSequence:Map[ChromosomeType,Chromosome]
  def secondGenomeSequence:Map[ChromosomeType,Chromosome]
}
sealed trait AnimalGenome extends Genome with Savable[]{
  def sexualChromosomeCouple:SexualChromosomeCouple
  def firstSexualChromosome:SexualChromosome
  def secondSexualChromosome:SexualChromosome
  override val requiredChromosomes:Seq[ChromosomeType] = List(ChromosomeType.COMMON,
                                                            ChromosomeType.STRUCTURAL_ANIMAL,
                                                            ChromosomeType.LIFE_CYCLE,
                                                            ChromosomeType.FEEDING)
}
sealed trait PlantGenome extends Genome{
  override val requiredChromosomes:Seq[ChromosomeType] = List(ChromosomeType.COMMON,
                                                              ChromosomeType.STRUCTURAL_PLANT)
}


abstract class CommonGenome(override val autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple]) extends Genome{

  override def firstGenomeSequence: Map[ChromosomeType, Chromosome] = autosomeChromosomeCouples.mapValues(_coupleToUnit(_,1))
  override def secondGenomeSequence: Map[ChromosomeType, Chromosome] = autosomeChromosomeCouples.mapValues(_coupleToUnit(_,2))

  private def _coupleToUnit(c:ChromosomeCouple,n:Int):(Chromosome)= n match {
    case 1 => new BasicChromosomeImpl(c.firstChromosome.chromosomeType,c.firstChromosome.geneList)
    case 2 => new BasicChromosomeImpl(c.secondChromosome.chromosomeType,c.secondChromosome.geneList)
  }
}
object PlantGenome{
  def apply(autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple]): Genome = new PlantGenomeImpl(autosomeChromosomeCouples)
  private class PlantGenomeImpl(
                                 autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple]
                               ) extends CommonGenome(autosomeChromosomeCouples) with PlantGenome
}

object AnimalGenome{
  def apply(
             autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple],
             sexualChromosomeCouple: SexualChromosomeCouple): AnimalGenome = {
    new AnimalGenomeImpl(autosomeChromosomeCouples,sexualChromosomeCouple)
  }
  private class AnimalGenomeImpl(autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple],
                                 override val sexualChromosomeCouple: SexualChromosomeCouple)
    extends CommonGenome(autosomeChromosomeCouples) with AnimalGenome{
//    require(requiredChromosomes.toSet== autosomeChromosomeCouples.keySet,"Required chromosome are: "+requiredChromosomes)
    private def _coupleToUnit(c:SexualChromosomeCouple,n:Int):(SexualChromosome)= n match {
      case 1 => new SexualChromosomeImpl(c.firstChromosome.chromosomeType,c.firstChromosome.sexualChromosome,c.firstChromosome.geneList)
      case 2 => new SexualChromosomeImpl(c.secondChromosome.chromosomeType,c.secondChromosome.sexualChromosome,c.secondChromosome.geneList)
    }

    override def firstSexualChromosome: SexualChromosome = _coupleToUnit(sexualChromosomeCouple,1)

    override def secondSexualChromosome: SexualChromosome = _coupleToUnit(sexualChromosomeCouple,2)

    override def toString: String = "First: "+firstGenomeSequence.values.toString()+"\n"+
                                    "Second: "+secondGenomeSequence.values.toString()+"\n"+
                                    "Sexual Chromosome 1: "+firstSexualChromosome.geneList.toString()+"\n"+
                                    "Sexual Chromosome 2: "+secondSexualChromosome.geneList.toString()+"\n"

  }
}

case class AnimalGenomeMemento(autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple],
                               sexualChromosomeCouple: SexualChromosomeCouple) extends Memento
