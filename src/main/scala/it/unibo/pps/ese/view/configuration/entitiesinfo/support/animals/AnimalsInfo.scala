package it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals

case class AnimalBaseInfo(geneLength: Int,
                          alleleLength: Int,
                          typology: String)

case class AnimalChromosomeInfo(var structuralChromosome: Map[String, CustomChromosomeInfo],
                                var regulationChromosome: Map[String, DefaultChromosomeInfo],
                                var sexualChromosome: Map[String, DefaultChromosomeInfo])

case class AnimalInfo(animalBaseInfo: AnimalBaseInfo,
                      animalChromosomeInfo: AnimalChromosomeInfo)

class ChromosomeInfo(val geneInfo: GeneInfo,
                     val alleles: Map[String, AlleleInfo])

case class CustomChromosomeInfo(override val geneInfo: CustomGeneInfo,
                                override val alleles: Map[String, AlleleInfo]) extends ChromosomeInfo(geneInfo, alleles)


case class DefaultChromosomeInfo(override val geneInfo: DefaultGeneInfo,
                                 override val alleles: Map[String, AlleleInfo]) extends ChromosomeInfo(geneInfo, alleles)

