package it.unibo.pps.ese.genetics


trait TranslatedAnimalData{
  def name: String
  def geneLength: Int
  def reign: String
  def typology: String
  def structuralChromosome: Seq[GeneData]
  def regulationChromosome: Seq[GeneData]
  def sexualChromosome: Seq[GeneData]
}
case class TranslatedAnimalDataImpl(
 name: String,
 geneLength: Int,
 reign: String,
 typology: String,
 structuralChromosome: Seq[GeneData],
 regulationChromosome: Seq[GeneData],
 sexualChromosome: Seq[GeneData]
) extends TranslatedAnimalData

