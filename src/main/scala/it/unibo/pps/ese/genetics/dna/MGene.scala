package it.unibo.pps.ese.genetics.dna

import it.unibo.pps.ese.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

sealed trait GeneType
case object IdentifierGene extends GeneType
case object StructuralGene extends GeneType
case object RegulatorGene extends GeneType

trait MGene{
  def geneType:GeneType
  def geneId:Seq[ProteinoGenicAmminoacid]
  def completeCode:Seq[ProteinoGenicAmminoacid]
  override def toString: String = "Gene amminoacid: "+geneId.toString()
}

case class BasicGene(
                      override val geneId:Seq[ProteinoGenicAmminoacid],
                      override val geneType: GeneType
                    ) extends MGene{
  override def completeCode: Seq[ProteinoGenicAmminoacid] = geneId
}

case class GeneWithAllelicForms(
                                 override val geneId:Seq[ProteinoGenicAmminoacid],
                                 alleleCode:Seq[ProteinoGenicAmminoacid],
                                 override val geneType: GeneType) extends MGene{
  override def completeCode: Seq[ProteinoGenicAmminoacid] = geneId ++ alleleCode
  override def toString: String = "{ "+geneId+",allelic amminoacid: "+alleleCode+"}"
}
