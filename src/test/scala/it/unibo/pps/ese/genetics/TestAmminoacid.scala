package it.unibo.pps.ese.genetics

import org.scalatest.FunSuite

class TestAmminoacid extends FunSuite{
  import AmminoAcidUtilities._
  import ProteinoGenicAmminoacid._
  test("Test Essential Amminoacid creation from char"){
    def checkAmminoacidEquality(p1:ProteinoGenicAmminoacid,p2:ProteinoGenicAmminoacid):Boolean= p1 == p2
    assert(checkAmminoacidEquality('A',ProteinoGenicAmminoacid.Ala))
    assertThrows[IllegalArgumentException](checkAmminoacidEquality('1',ProteinoGenicAmminoacid.Ala))
  }
  test("Test the implicit creation of Essential Amminoacid from char seq"){
    def checkAmminoacidEquality(s1:Seq[ProteinoGenicAmminoacid],s2:Seq[ProteinoGenicAmminoacid]):Boolean= s1 == s2
    assert(checkAmminoacidEquality(List('A','C'),List(ProteinoGenicAmminoacid.Ala,ProteinoGenicAmminoacid.Cys)))
    assertThrows[IllegalArgumentException](checkAmminoacidEquality(List('A','1'),List(ProteinoGenicAmminoacid.Ala)))
  }

}
