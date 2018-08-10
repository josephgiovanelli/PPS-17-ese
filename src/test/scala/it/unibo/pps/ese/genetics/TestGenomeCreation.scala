package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import org.scalatest.FunSuite
import AmminoAcidUtilities._
class TestGenomeCreation extends FunSuite{
  test("Test creation"){
    //Common CHROMOSOME
    val cr:Seq[ProteinoGenicAmminoacid] = List('A')
    val cs:Seq[ProteinoGenicAmminoacid] = List('T')
    val gr:BasicGene = BasicGene(cr,IdentifierGene)
    val gs:BasicGene = BasicGene(cs,IdentifierGene)
    //Structural CHROMOSOME
    //geneBaseCode
    val cgs1:Seq[ProteinoGenicAmminoacid] = List('G')
    //allelesCode
    val cas1:Seq[ProteinoGenicAmminoacid] = List('L')
    val cas2:Seq[ProteinoGenicAmminoacid] = List('M')
    val cas3:Seq[ProteinoGenicAmminoacid] = List('N')
    val cas4:Seq[ProteinoGenicAmminoacid] = List('O')

    val sap1:AlleleWithProbability = AlleleWithProbability(cas1,0.25)
    val sap2:AlleleWithProbability = AlleleWithProbability(cas2,0.25)
    val sap3:AlleleWithProbability = AlleleWithProbability(cas3,0.25)
    val sap4:AlleleWithProbability = AlleleWithProbability(cas4,0.25)

    val sgp:GeneWithPossibleAlleles= GeneWithPossibleAlleles(cgs1,List(sap1,sap2,sap3,sap4))

    //LIFE CYCLE CHROMOSOME
    val lcgs1:Seq[ProteinoGenicAmminoacid] = List('T')
    val lcgs2:Seq[ProteinoGenicAmminoacid] = List('U')
    val lcgs3:Seq[ProteinoGenicAmminoacid] = List('V')
    val lcgs4:Seq[ProteinoGenicAmminoacid] = List('W')
//    val lcgs5:Seq[ProteinoGenicAmminoacid] = List('Y')


    //allelesCode
    val lcgs1a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val lcgs1a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val lcgs2a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val lcgs2a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val lcgs3a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val lcgs3a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val lcgs4a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val lcgs4a2:Seq[ProteinoGenicAmminoacid] = List('Q')

//    val lcgs5a1:Seq[ProteinoGenicAmminoacid] = List('P')
//    val lcgs5a2:Seq[ProteinoGenicAmminoacid] = List('Q')


    val lcap11:AlleleWithProbability = AlleleWithProbability(lcgs1a1,0.5)
    val lcap12:AlleleWithProbability = AlleleWithProbability(lcgs1a2,0.5)

    val lcap21:AlleleWithProbability = AlleleWithProbability(lcgs2a1,0.5)
    val lcap22:AlleleWithProbability = AlleleWithProbability(lcgs2a2,0.5)

    val lcap31:AlleleWithProbability = AlleleWithProbability(lcgs3a1,0.5)
    val lcap32:AlleleWithProbability = AlleleWithProbability(lcgs3a2,0.5)

    val lcap41:AlleleWithProbability = AlleleWithProbability(lcgs4a1,0.5)
    val lcap42:AlleleWithProbability = AlleleWithProbability(lcgs4a2,0.5)

//    val lcap51:AlleleWithProbability = AlleleWithProbability(lcgs5a1,0.5)
//    val lcap52:AlleleWithProbability = AlleleWithProbability(lcgs5a2,0.5)

    val lcgp1:GeneWithPossibleAlleles= GeneWithPossibleAlleles(lcgs1,List(lcap11,lcap12))
    val lcgp2:GeneWithPossibleAlleles= GeneWithPossibleAlleles(lcgs2,List(lcap21,lcap22))
    val lcgp3:GeneWithPossibleAlleles= GeneWithPossibleAlleles(lcgs3,List(lcap31,lcap32))
    val lcgp4:GeneWithPossibleAlleles= GeneWithPossibleAlleles(lcgs4,List(lcap41,lcap42))
//    val lcgp5:GeneWithPossibleAlleles= GeneWithPossibleAlleles(lcgs5,List(lcap51,lcap52))

    //FEEDING CHROMOSOME
    val cfr:Seq[ProteinoGenicAmminoacid] = List('D','C')

    val fg:BasicGene = BasicGene(cfr,IdentifierGene)

    //SEXUAL CHROMOSOME
    val sxgs1:Seq[ProteinoGenicAmminoacid] = List('S')
    val sxgs2:Seq[ProteinoGenicAmminoacid] = List('R')

    //allelesCode
    val sxgs1a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val sxgs1a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val sxgs2a1:Seq[ProteinoGenicAmminoacid] = List('P')
    val sxgs2a2:Seq[ProteinoGenicAmminoacid] = List('Q')

    val sxap11:AlleleWithProbability = AlleleWithProbability(sxgs1a1,0.5)
    val sxap12:AlleleWithProbability = AlleleWithProbability(sxgs1a2,0.5)

    val sxap21:AlleleWithProbability = AlleleWithProbability(sxgs2a1,0.5)
    val sxap22:AlleleWithProbability = AlleleWithProbability(sxgs2a2,0.5)

    val sxgp1:GeneWithPossibleAlleles= GeneWithPossibleAlleles(sxgs1,List(sxap11,sxap12))
    val sxgp2:GeneWithPossibleAlleles= GeneWithPossibleAlleles(sxgs2,List(sxap21,sxap22))

    val speciesGenerator:SpeciesGenerator = new SpeciesGenerator(
      commonChromosomeGenes = List(gr,gs),
      structuralChromosomeGenes = List(sgp),
      lifeCycleChromosomeGenes = List(lcgp1,lcgp2,lcgp3,lcgp4,lcgp4),
      feedingChromosomeGenes = List(fg),
      sexualChromosomeGenes = List(sxgp1,sxgp2)
    )
    println(speciesGenerator.generateAnimalGenome)

//    val g1:Seq[ProteinoGenicAmminoacid] = List('A')
//    val a1:Seq[ProteinoGenicAmminoacid] = List('C')
//    val a2:Seq[ProteinoGenicAmminoacid] = List('D')
//    val a3:Seq[ProteinoGenicAmminoacid] = List('F')
//    val ap1:AlleleWithProbability = AlleleWithProbability(a1,0.4)
//    val ap2:AlleleWithProbability = AlleleWithProbability(a2,0.3)
//    val ap3:AlleleWithProbability = AlleleWithProbability(a3,0.3)
//    val gpa:GeneWithPossibleAlleles = GeneWithPossibleAlleles(g1,List(ap1,ap2,ap3))
//    val map = gpa.alleles.map(a=>(a.allelicSeq,a.probability)).toMap
//    println(map)
//    println(Utilities.sample(map))
//    println(Vector.fill(1000)(Utilities.sample(map)).groupBy(identity).mapValues(_.size))
//

  }
}
