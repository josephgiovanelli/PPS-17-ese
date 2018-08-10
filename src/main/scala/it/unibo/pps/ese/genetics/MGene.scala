package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

sealed trait GeneType
case object IdentifierGene extends GeneType
case object StructuralGene extends GeneType
case object RegulatorGene extends GeneType

trait MGene{
  def geneType:GeneType
  def geneCode:Seq[ProteinoGenicAmminoacid]
  def completeCode:Seq[ProteinoGenicAmminoacid]
  override def toString: String = "Gene amminoacid: "+geneCode.toString()
}

case class BasicGene(
                      override val geneCode:Seq[ProteinoGenicAmminoacid],
                      override val geneType: GeneType
                    ) extends MGene{
  override def completeCode: Seq[ProteinoGenicAmminoacid] = geneCode
}

case class GeneWithAllelicForms(
                                 override val geneCode:Seq[ProteinoGenicAmminoacid],
                                 alleleCode:Seq[ProteinoGenicAmminoacid],
                                 override val geneType: GeneType) extends MGene{
  override def completeCode: Seq[ProteinoGenicAmminoacid] = geneCode ++ alleleCode
  override def toString: String = "{ "+geneCode+",allelic amminoacid: "+alleleCode+"}"
}
