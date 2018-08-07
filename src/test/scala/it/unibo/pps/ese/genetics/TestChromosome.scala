package it.unibo.pps.ese.genetics

import org.scalatest.FunSuite

class TestChromosome extends FunSuite{
  test("Test the creation of various chromosome"){
    import AmminoAcidUtilities._
    val gs= GeneWithAllelicForms(List('A'),List('A'),RegulatorGene)
    val gs2 = GeneWithAllelicForms(List('A'),List('F'),RegulatorGene)
  }
}
