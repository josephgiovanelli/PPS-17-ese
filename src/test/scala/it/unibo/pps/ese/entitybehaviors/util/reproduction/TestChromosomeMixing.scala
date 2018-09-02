package it.unibo.pps.ese.entitybehaviors.util.reproduction

import it.unibo.pps.ese.genetics.dna.{AnimalGenome, BasicGene, Chromosome, ChromosomeCouple, ChromosomeType, MGene, ProteinoGenicAmminoacid, RegulatorGene, SexualChromosomeCouple, StructuralGene, X, Y}
import it.unibo.pps.ese.genetics.entities.AnimalInfo
import org.scalatest.FunSuite

class TestChromosomeMixing extends FunSuite {

  implicit val fake: GeneticsEngine = new GeneticsEngine {
    override def getAnimalInfoByGenome(species: String, childGenome: AnimalGenome): AnimalInfo = null

    override def obtainMutantAlleles(species: String, gene: MGene): Iterable[MGene] = Seq()

    override def mutationProb: Double = 0
  }

  test("Chromosomes couples can be correctly mixed without mutations") {
    val bg1 = BasicGene(Seq('A'), StructuralGene)
    val bg2 = BasicGene(Seq('B'), StructuralGene)
    val bg3 = BasicGene(Seq('C'), StructuralGene)
    val bg4 = BasicGene(Seq('D'), StructuralGene)
    val c1 = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL, bg1, bg2)
    val c2 = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL, bg3, bg4)

    val couple1 = ChromosomeCouple(c1, c1)
    val couple2 = ChromosomeCouple(c2, c2)

    val sonCouple = EmbryosUtil.generateNewChromosomeCouple(couple1, couple2, "test")
    require(sonCouple._1 == ChromosomeType.STRUCTURAL_ANIMAL)
    assert(sonCouple._2.firstChromosome == c1)
    assert(sonCouple._2.secondChromosome == c2)

    val sg1 = BasicGene(Seq('A'), RegulatorGene)
    val sg2 = BasicGene(Seq('B'), RegulatorGene)
    val sg3 = BasicGene(Seq('C'), RegulatorGene)
    val sg4 = BasicGene(Seq('D'), RegulatorGene)
    val sc1 = Chromosome(ChromosomeType.SEXUAL_X, X, sg1, sg4)
    val sc2 = Chromosome(ChromosomeType.SEXUAL_Y, Y)

    val sexualCouple1 = SexualChromosomeCouple(sc1, sc1)
    val sexualCouple2 = SexualChromosomeCouple(sc1, sc1)

    val sonSexCouple = EmbryosUtil.generateNewChromosomeCouple(sexualCouple1, sexualCouple2, "test")
    assert(sonSexCouple.firstChromosome == sc1)
  }

  test("Chromosomes couples can be correctly mixed with mutations") {

  }
}
