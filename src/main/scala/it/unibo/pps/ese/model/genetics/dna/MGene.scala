package it.unibo.pps.ese.model.genetics.dna

import it.unibo.pps.ese.model.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

sealed trait GeneType
case object IdentifierGene extends GeneType{
  override def toString: String = "Identifier Gene"
}
case object StructuralGene extends GeneType{
  override def toString: String = "Structural Gene"
}
case object RegulatorGene extends GeneType{
  override def toString: String = "Regulator Gene"
}

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
