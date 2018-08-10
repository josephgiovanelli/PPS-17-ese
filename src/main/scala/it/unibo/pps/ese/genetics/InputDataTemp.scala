package it.unibo.pps.ese.genetics


trait MyAnimalData{
  def name: String
  def geneLength: Int
  def reign: String
  def typology: String
  def structuralChromosome: Seq[GeneData]
  def regulationChromosome: Seq[GeneData]
  def sexualChromosome: Seq[GeneData]
}
case class MyAnimalDataImpl(
 name: String,
 geneLength: Int,
 reign: String,
 typology: String,
 structuralChromosome: Seq[GeneData],
 regulationChromosome: Seq[GeneData],
 sexualChromosome: Seq[GeneData]
) extends MyAnimalData

