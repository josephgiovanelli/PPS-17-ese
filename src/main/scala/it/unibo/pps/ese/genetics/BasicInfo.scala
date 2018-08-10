package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import AmminoAcidUtilities._
trait Identified
sealed trait DietType extends Identified{
  def dietName:String
  def geneId:BasicGene
}
case object Herbivore extends DietType{
  val dietName:String = "H"
  val geneId:BasicGene = BasicGene(List('D','E'),IdentifierGene)
}
case object Carnivorous extends DietType{
  val dietName:String = "C"
  val geneId:BasicGene = BasicGene(List('D','C'),IdentifierGene)
}

sealed trait Gender
case object Male extends Gender
case object Female extends Gender

sealed trait Reign{
  def reignName:String
  def geneId:BasicGene
}
case object Animal extends Reign{
  val reignName ="A"
  val geneId:BasicGene = BasicGene(List('A'),IdentifierGene)
}
case object Plant extends Reign{
  val reignName="P"
  val geneId:BasicGene = BasicGene(List('P'),IdentifierGene)
}
