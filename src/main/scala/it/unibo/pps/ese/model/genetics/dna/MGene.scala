package it.unibo.pps.ese.model.genetics.dna

import it.unibo.pps.ese.model.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

/**
  * The type of Gene
  */
sealed trait GeneType

/**
  * A Gene that identify something
  */
case object IdentifierGene extends GeneType{
  override def toString: String = "Identifier Gene"
}

/**
  * A gene that encode a body part
  */
case object StructuralGene extends GeneType{
  override def toString: String = "Structural Gene"
}

/**
  * A gene that regulate some [[it.unibo.pps.ese.model.genetics.entities.Quality]]
  */
case object RegulatorGene extends GeneType{
  override def toString: String = "Regulator Gene"
}

/**
  * An abstraction for a Gene
  */
trait MGene{
  /**
    *
    * @return the [[GeneType]] of this Gene
    */
  def geneType:GeneType

  /**
    * GeneID
    * @return the sequence of [[ProteinoGenicAmminoacid]] that identity the gene ( but not the allelic form)
    */
  def geneId:Seq[ProteinoGenicAmminoacid]
  /**
    * GeneID ++ AlleleID ( if existent)
    * @return the sequence of [[ProteinoGenicAmminoacid]] that identity the gene and the allelic form
    */
  def completeCode:Seq[ProteinoGenicAmminoacid]
  override def toString: String = "Gene amminoacid: "+geneId.toString()
}

/**
  * A basic [[MGene]] without allelic forms
  * @param geneId
  * @param geneType
  */
case class BasicGene(
                      override val geneId:Seq[ProteinoGenicAmminoacid],
                      override val geneType: GeneType
                    ) extends MGene{
  override def completeCode: Seq[ProteinoGenicAmminoacid] = geneId
}

/**
  * A [[MGene]] with allelic form
  * @param geneId
  * @param alleleCode
  * @param geneType
  */
case class GeneWithAllelicForms(
                                 override val geneId:Seq[ProteinoGenicAmminoacid],
                                 alleleCode:Seq[ProteinoGenicAmminoacid],
                                 override val geneType: GeneType) extends MGene{
  override def completeCode: Seq[ProteinoGenicAmminoacid] = geneId ++ alleleCode
  override def toString: String = "{ "+geneId+",allelic amminoacid: "+alleleCode+"}"
}
