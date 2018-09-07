package it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals

case class AnimalBaseInfo(geneLength: Int,
                          alleleLength: Int,
                          typology: String)

case class AnimalChromosomeInfo(var structuralChromosome: Map[String, (CustomGeneInfo, Map[String, AlleleInfo])],
                                var regulationChromosome: Map[String, (DefaultGeneInfo, Map[String, AlleleInfo])],
                                var sexualChromosome: Map[String, (DefaultGeneInfo, Map[String, AlleleInfo])])

case class AnimalInfo(animalBaseInfo: AnimalBaseInfo, animalChromosomeInfo: AnimalChromosomeInfo)

