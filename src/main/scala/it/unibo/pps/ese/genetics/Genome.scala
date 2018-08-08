package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ChromosomeType.ChromosomeType

sealed trait Genome{
  def autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple]
  def firstGenomeSequence:Map[ChromosomeType,Chromosome]
  def secondGenomeSequence:Map[ChromosomeType,Chromosome]
}
sealed trait AnimalGenome extends Genome{
  def sexualChromosomeCouple:SexualChromosomeCouple
  def firstSexualChromosome:SexualChromosome
  def secondSexualChromosome:SexualChromosome
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
                               ) extends CommonGenome(autosomeChromosomeCouples)
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
    private def _coupleToUnit(c:SexualChromosomeCouple,n:Int):(SexualChromosome)= n match {
      case 1 => new SexualChromosomeImpl(c.firstChromosome.chromosomeType,c.firstChromosome.sexualChromosome,c.firstChromosome.geneList)
      case 2 => new SexualChromosomeImpl(c.secondChromosome.chromosomeType,c.secondChromosome.sexualChromosome,c.secondChromosome.geneList)
    }

    override def firstSexualChromosome: SexualChromosome = _coupleToUnit(sexualChromosomeCouple,1)

    override def secondSexualChromosome: SexualChromosome = _coupleToUnit(sexualChromosomeCouple,2)
  }
}