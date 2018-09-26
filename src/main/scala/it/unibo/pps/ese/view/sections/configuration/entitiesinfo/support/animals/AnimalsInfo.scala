package it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals

/**
  * It describes the animal base info.
  * @param geneLength the length of gene identifiers
  * @param alleleLength the length of allele identifier
  * @param typology the typology can be carnivorous or herbivore
  */
case class AnimalBaseInfo(geneLength: Int,
                          alleleLength: Int,
                          typology: String)

/**
  * It describes the chromosome of the animal.
  * @param structuralChromosome the custom structural chromosome
  * @param regulationChromosome the default chromosome that regulates the life
  * @param sexualChromosome the default chromosome that regulates the sexual life
  */
case class AnimalChromosomeInfo(var structuralChromosome: Map[String, CustomChromosomeInfo],
                                var regulationChromosome: Map[String, DefaultChromosomeInfo],
                                var sexualChromosome: Map[String, DefaultChromosomeInfo])

/**
  * It describes an animal.
  * @param animalBaseInfo the animal base info
  * @param animalChromosomeInfo the animal chromosome
  */
case class AnimalInfo(animalBaseInfo: AnimalBaseInfo,
                      animalChromosomeInfo: AnimalChromosomeInfo)

/**
  * It describes a genes of a general chromosome
  * @param geneInfo the information about the gene.
  * @param alleles the information about the alleles
  */
class ChromosomeInfo(val geneInfo: GeneInfo,
                     val alleles: Map[String, AlleleInfo])

/**
  * It describes a custom gene (that belongs to Structural Chromosome).
  * @param geneInfo the information about the gene.
  * @param alleles the information about the alleles
  */
case class CustomChromosomeInfo(override val geneInfo: CustomGeneInfo,
                                override val alleles: Map[String, AlleleInfo]) extends ChromosomeInfo(geneInfo, alleles)

/**
  * It describes a default gene (that belongs to Regulation or Sexual Chromosome).
  * @param geneInfo the information about the gene.
  * @param alleles the information about the alleles
  */
case class DefaultChromosomeInfo(override val geneInfo: DefaultGeneInfo,
                                 override val alleles: Map[String, AlleleInfo]) extends ChromosomeInfo(geneInfo, alleles)

