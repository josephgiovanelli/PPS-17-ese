package it.unibo.pps.ese.genetics

object ChromosomeType extends Enumeration {
  type ChromosomeType = Value
  protected case class Val(trueSequence: Seq[GeneType]) extends super.Val{
  }
  implicit def valueToChromosomeType(x: Value): Val = x.asInstanceOf[Val]

  val COMMON = Val(List(IdentifierGene,IdentifierGene))
  val STRUCTURAL_PLANT = Val(List(StructuralGene,StructuralGene,StructuralGene))
  val ONLY_FOR_TEST = Val(List(RegulatorGene))
  val STRUCTURAL_ANIMAL = Val(List())
  val LIFE_CYCLE = Val(List(RegulatorGene,RegulatorGene,RegulatorGene,RegulatorGene,RegulatorGene))
  val FEEDING = Val(List(IdentifierGene))
  val SEXUAL_X = Val(List(RegulatorGene,RegulatorGene))
  val SEXUAL_Y = Val(List())
  def checkListOfGene(chromosomeType: ChromosomeType,seq: Seq[GeneType]):Boolean= chromosomeType match {
    case STRUCTURAL_ANIMAL => seq.forall(_ == StructuralGene)
    case _ =>chromosomeType.trueSequence.equals(seq)
  }
}


sealed trait SexualChromosomeType
case object X extends SexualChromosomeType
case object Y extends SexualChromosomeType



import ChromosomeType.{ChromosomeType, _}

case class CommonChromosome(reignGene:BasicGene,speciesGene:BasicGene)

trait ChromosomeCouple{
  type ChromosomeUnit <: {
    def geneList: Seq[MGene]
    def chromosomeType:ChromosomeType
  }
  def addChromosomeCouple(c1:ChromosomeUnit, c2:ChromosomeUnit)
  def addFirstChromosome(c:ChromosomeUnit)
  def addSecondChromosome(c:ChromosomeUnit)
  def firstChromosome:ChromosomeUnit
  def secondChromosome:ChromosomeUnit
}
object ChromosomeCouple{
  def apply(c1: Chromosome,c2: Chromosome): ChromosomeCouple = {
    val cc = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    cc.addChromosomeCouple(c1,c2)
    cc
  }
}

trait SexualChromosomeCouple extends ChromosomeCouple{
  type ChromosomeUnit <: {
    def sexualChromosome:SexualChromosomeType
    def geneList: Seq[MGene]
    def chromosomeType:ChromosomeType
  }
  def gender:Gender = (firstChromosome.sexualChromosome,secondChromosome.sexualChromosome) match {
    case (X,X) => Female
    case (X,Y) => Male
    case (Y,X) => Male
    case _=> throw new IllegalStateException()
  }
}
object exualChromosomeCouple{
  def apply(sc1:SexualChromosome,sc2:SexualChromosome): SexualChromosomeCouple ={
    val scc = new ChromosomeCoupleImpl with SexualChromosomeCouple {
      type ChromosomeUnit = SexualChromosome
    }
    scc.addChromosomeCouple(sc1,sc2)
    scc
  }
}

sealed trait Chromosome{
  def chromosomeType:ChromosomeType
  def geneList:Seq[MGene]
}

sealed trait SexualChromosome extends Chromosome{
  def sexualChromosome: SexualChromosomeType
}

abstract class ChromosomeCoupleImpl() extends ChromosomeCouple{
  private var couple:Map[Int,ChromosomeUnit] = Map()
  override def addChromosomeCouple(c1: ChromosomeUnit, c2: ChromosomeUnit): Unit = {
    _assignCouple(c1,c2)
  }
  override def addFirstChromosome(c: ChromosomeUnit): Unit = _assignCouple(c,couple(2))

  override def addSecondChromosome(c: ChromosomeUnit): Unit = _assignCouple(couple(1),c)

  override def firstChromosome: ChromosomeUnit = couple(1)

  override def secondChromosome: ChromosomeUnit = couple(2)

  private def _checkConsistency(c1:ChromosomeUnit, c2:ChromosomeUnit):Boolean = {
    c1.chromosomeType==c2.chromosomeType ||
    c1.chromosomeType == ChromosomeType.SEXUAL_X||
    c1.chromosomeType == ChromosomeType.SEXUAL_Y
  }

  private def _assignCouple(c1:ChromosomeUnit, c2:ChromosomeUnit):Unit = _checkConsistency(c1,c2) match {
    case true=> couple = Map(1->c1,2->c2)
    case _=> throw new IllegalArgumentException()
  }
}

abstract class BasicChromosome(
                                override val chromosomeType: ChromosomeType,
                                override val geneList:Seq[MGene]) extends Chromosome{
//  println(chromosomeType+geneList.map(_.geneType).toString())
  require(checkListOfGene(chromosomeType,geneList.map(_.geneType)))
}

class BasicChromosomeImpl(chromosomeType: ChromosomeType,geneList:Seq[MGene])
  extends BasicChromosome(chromosomeType,geneList){
  override def toString: String = chromosomeType.toString+"->"+geneList
}

class SexualChromosomeImpl(chromosomeType: ChromosomeType,
                           override val sexualChromosome: SexualChromosomeType,
                           geneList:Seq[MGene])
  extends BasicChromosome(chromosomeType,geneList) with SexualChromosome{}

object Chromosome{
  def apply(chromosomeType: ChromosomeType,geneList:MGene*): Chromosome = new BasicChromosomeImpl(chromosomeType,geneList)

  def apply(chromosomeType: ChromosomeType,
            sexualChromosome: SexualChromosomeType,
            geneList:MGene*): SexualChromosome = new SexualChromosomeImpl(chromosomeType,
    sexualChromosome,
    geneList)
}

