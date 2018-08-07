package it.unibo.pps.ese.genetics
object Gene{

  import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

  sealed trait GeneType
  case object IdentifierGene extends GeneType
  case object StructuralGene extends GeneType
  case object RegulatorGene extends GeneType

  trait Gene{
    def geneType:GeneType
    def geneCode:Seq[ProteinoGenicAmminoacid]
    override def toString: String = "Gene amminoacid: "+geneCode.toString()
  }

  case class BasicGene(
                        override val geneCode:Seq[ProteinoGenicAmminoacid],
                        override val geneType: GeneType
                      ) extends Gene{
  }

  case class GeneWithAllelicForms(
                                   geneSequence:Seq[ProteinoGenicAmminoacid],
                                   alleleCode:Seq[ProteinoGenicAmminoacid],
                                   override val geneType: GeneType) extends Gene{
    override def geneCode: Seq[ProteinoGenicAmminoacid] = geneSequence ++ alleleCode
    override def toString: String = "{ "+geneSequence+",allelic amminoacid: "+alleleCode+"}"
  }

}
