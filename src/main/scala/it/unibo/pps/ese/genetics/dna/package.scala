package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.genetics.entities._

package object dna {
  def stringToElement[T](s1:String,s2:String,r1:T,r2:T)(s:String):T = s match {
    case `s1` => r1
    case `s2` => r2
  }

  implicit def stringToReign(s:String):Reign = {
    stringToElement(Animal.reignName,Plant.reignName,Animal,Plant)(s)
  }
  implicit def stringToReignGene(s:String):BasicGene = {
    stringToReign(s).geneId
  }

  def stringToDiet(s: String): BasicGene = {
    stringToElement(Herbivore.dietName,Carnivorous.dietName,Herbivore.geneId,Carnivorous.geneId)(s)
  }

  def speciesNameToGene(s: String): BasicGene = {
    BasicGene(amminoAcidSeqFromString(s), IdentifierGene)
  }

  implicit def charToAmminoacid(c:Char):ProteinoGenicAmminoacid = {
    ProteinoGenicAmminoacid.values.find(p=>p.shortName == c).getOrElse(throw new IllegalArgumentException)
  }
  implicit def seqCharToListAmminoacid(seq: Seq[Char]):Seq[ProteinoGenicAmminoacid] = {
    seq.map(charToAmminoacid)
  }
  def amminoAcidSeqFromString(s:String):Seq[ProteinoGenicAmminoacid]=
    s.toUpperCase
      .toSeq
      .filter(c=>ProteinoGenicAmminoacid
        .values
        .map(_.shortName)
        .contains(c))
      .toList

  implicit class RichChromosome (c1 :Chromosome) {
    def :+:(c2:Chromosome):ChromosomeCouple = ChromosomeCouple(c1,c2)
    def |->|(c2:Chromosome):(ChromosomeType,ChromosomeCouple) = c1.chromosomeType->ChromosomeCouple(c1,c2)
  }
  implicit class RichSexualChromosome (c1 :SexualChromosome) {
    def :+:(c2:SexualChromosome):SexualChromosomeCouple = SexualChromosomeCouple(c1,c2)
  }
  implicit class RichChromosomeMap(aC:Map[ChromosomeType,ChromosomeCouple]){
    def |%-%|(scc: SexualChromosomeCouple):AnimalGenome=dna.AnimalGenome(aC,scc)
  }
  implicit class RichGenome(genome: AnimalGenome){
    def coupledGene:Map[ChromosomeType,List[(MGene,MGene)]] = {
      genome.firstGenomeSequence.values.map(c=> {
        val geneCouples:List[(MGene,MGene)] = c.geneList.map(g=>{
          val specular = genome.secondGenomeSequence(c.chromosomeType).geneList.find(_.geneId==g.geneId).get
          (g,specular)
        }).toList
        c.chromosomeType->geneCouples
      }
      ).toMap
    }
  }
}
