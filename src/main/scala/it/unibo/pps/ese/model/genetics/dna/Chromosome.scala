package it.unibo.pps.ese.model.genetics.dna

/**
  * Enumeration that contains all the possible type of Chromosome each with the list of the type of genes required
  */
object ChromosomeType extends Enumeration {
  type ChromosomeType = Value
  protected case class Val(trueSequence: Seq[GeneType],name:String) extends super.Val{
    override def toString(): String = name
  }
  implicit def valueToChromosomeType(x: Value): Val = x.asInstanceOf[Val]

  val COMMON = Val(List(IdentifierGene,IdentifierGene),"Common Chromosome")
  val STRUCTURAL_PLANT = Val(List(StructuralGene,StructuralGene,StructuralGene),"Structural Chromosome")
  val ONLY_FOR_TEST = Val(List(RegulatorGene),"Test")
  val STRUCTURAL_ANIMAL = Val(List(),"Structural Chromosome")
  val LIFE_CYCLE = Val(List(RegulatorGene,RegulatorGene,RegulatorGene,RegulatorGene,RegulatorGene),"Life Cycle Chromosome")
  val FEEDING = Val(List(IdentifierGene),"Feeding Chromosome")
  val SEXUAL_X = Val(List(RegulatorGene,RegulatorGene,RegulatorGene),"X Chromosome")
  val SEXUAL_Y = Val(List(),"Y Chromosome")
  def checkListOfGene(chromosomeType: ChromosomeType,seq: Seq[GeneType]):Boolean= chromosomeType match {
    case STRUCTURAL_ANIMAL => seq.forall(_ == StructuralGene)
    case _ =>chromosomeType.trueSequence.equals(seq)
  }

}

sealed trait SexualChromosomeType
case object X extends SexualChromosomeType
case object Y extends SexualChromosomeType


import it.unibo.pps.ese.model.genetics.dna.ChromosomeType.{ChromosomeType, _}

/**
  * The abstraction adopted for a Chromosome
  */
sealed trait Chromosome{ self =>
  /**
    *
    * @return The [[ChromosomeType]] of this chromosome
    */
  def chromosomeType:ChromosomeType

  /**
    * @return The list of [[MGene]] in this chromosome
    */
  def geneList:Seq[MGene]

  /**
    * To mutate the chromosome with a new gene list
    * @param newGenes
    *                 The new gene list
    * @return
    *         The mutated chromosome
    */
  def mutate(newGenes: Seq[MGene]): self.type
}

/**
  * Companion object of Chromosome
  */
object Chromosome{
  /**
    * To build a Chromosome
    * @param chromosomeType
    * @param geneList
    * @return
    */
  def apply(chromosomeType: ChromosomeType,geneList:MGene*): Chromosome =
    new BasicChromosomeImpl(chromosomeType,geneList)

  /**
    * To build a sexual Chromosome
    * @param chromosomeType
    *                         The [[ChromosomeType]] of the Chromosome
    * @param sexualChromosome
    *                         The [[SexualChromosomeType]] of the Chromosome
    * @param geneList
    *                 The [[MGene]] list in the chromosome
    * @return
    */
  def apply(chromosomeType: ChromosomeType,
            sexualChromosome: SexualChromosomeType,
            geneList:MGene*): SexualChromosome = new SexualChromosomeImpl(chromosomeType,
    sexualChromosome,
    geneList)
}

/**
  * A sexual chromosome
  */
sealed trait SexualChromosome extends Chromosome{
  def sexualChromosome: SexualChromosomeType
}

/**
  * An abstract implementation for [[Chromosome]] that checks if the list of genes is right
  * @param chromosomeType
  * @param geneList
  */
abstract class BasicChromosome(
                                override val chromosomeType: ChromosomeType,
                                override val geneList:Seq[MGene]) extends Chromosome{
  require(checkListOfGene(chromosomeType,geneList.map(_.geneType)))

  def canEqual(other: Any): Boolean = other.isInstanceOf[BasicChromosome]

  override def equals(other: Any): Boolean = other match {
    case that: BasicChromosome =>
      (that canEqual this) &&
        chromosomeType == that.chromosomeType &&
        geneList == that.geneList
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(chromosomeType, geneList)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = chromosomeType.toString+"->"+geneList
}

/**
  * A autosome Chromosome
  * @param chromosomeType
  * @param geneList
  */
class BasicChromosomeImpl(chromosomeType: ChromosomeType,geneList:Seq[MGene])
  extends BasicChromosome(chromosomeType,geneList){ self =>

  override def mutate(newGenes: Seq[MGene]) =
    new BasicChromosomeImpl(chromosomeType, newGenes).asInstanceOf[self.type ]
}

/**
  * A sexual chromosome
  * @param chromosomeType
  * @param sexualChromosome
  * @param geneList
  */
class SexualChromosomeImpl(chromosomeType: ChromosomeType,
                           override val sexualChromosome: SexualChromosomeType,
                           geneList:Seq[MGene])
  extends BasicChromosome(chromosomeType,geneList) with SexualChromosome{ self =>
  override def mutate(newGenes: Seq[MGene]) =
    new SexualChromosomeImpl(chromosomeType, sexualChromosome, newGenes).asInstanceOf[self.type]
}

