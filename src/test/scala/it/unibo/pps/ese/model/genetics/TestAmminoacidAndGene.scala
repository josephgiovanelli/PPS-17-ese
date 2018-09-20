package it.unibo.pps.ese.model.genetics

import it.unibo.pps.ese.model.genetics.dna._
import org.scalatest.FunSuite

class TestAmminoacidAndGene extends FunSuite{
  import it.unibo.pps.ese.model.genetics.dna.ProteinoGenicAmminoacid._
  test("Test Essential Amminoacid creation from char"){
    def checkAmminoacidEquality(p1:ProteinoGenicAmminoacid,p2:ProteinoGenicAmminoacid):Boolean= p1 == p2
    assert(checkAmminoacidEquality('A',ProteinoGenicAmminoacid.Ala))
    assertThrows[IllegalArgumentException](checkAmminoacidEquality('1',ProteinoGenicAmminoacid.Ala))
  }

  test("Test the implicit creation of Essential Amminoacid from char seq"){

    val l1:Seq[ProteinoGenicAmminoacid] = List('A','C')
    val l2:Seq[ProteinoGenicAmminoacid] = List(ProteinoGenicAmminoacid.Ala,ProteinoGenicAmminoacid.Cys)
    assert(l1==l2)
    val l3 = List('A','1')
    assertThrows[IllegalArgumentException](returnSizeOfSeq(l3))
    def returnSizeOfSeq(l:Seq[ProteinoGenicAmminoacid]):Int = l.size
  }

  test("Testing if the gene code returned by class that" +
    " extend trait Gene is the right one"){
    val simpleGeneSeq:Seq[ProteinoGenicAmminoacid] = List('F')
    val basicGene = BasicGene(simpleGeneSeq,IdentifierGene)
    assert(basicGene.geneId == simpleGeneSeq)

    val geneIdentifier:Seq[ProteinoGenicAmminoacid] = List('A','C')
    val alleleIdentifier:Seq[ProteinoGenicAmminoacid] = List('D')
    val geneWithAllelicForms = GeneWithAllelicForms(geneIdentifier,alleleIdentifier,StructuralGene)
    assert((geneIdentifier ++ alleleIdentifier) == geneWithAllelicForms.completeCode)
    assert(geneIdentifier ==geneWithAllelicForms.geneId)
    assert(alleleIdentifier == geneWithAllelicForms.alleleCode)
  }
}
