package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.controller.loader.data.{CustomGeneData, DefaultGeneData}

case class AnimalBaseInfo(geneLength: Int, alleleLength: Int, reign: String, typology: String)
case class AnimalChromosomeInfo(structuralChromosome: Iterable[CustomGeneData], regulationChromosome: Iterable[DefaultGeneData],
                                sexualChromosome: Iterable[DefaultGeneData])

case class PlantInfo(height: Double, nutritionalValue: Double, hardness: Double, availability: Double)

sealed trait EntitiesInfo {

  def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit

  def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit

  def getAnimalInfo(id: String): Option[(AnimalBaseInfo, AnimalChromosomeInfo)]

  def setPlantInfo(id: String, plantInfo: PlantInfo): Unit

  def getPlantInfo(id: String): Option[PlantInfo]
}

object EntitiesInfo {
  private val _instance = new EntitiesInfoImpl()
  def instance() =
    _instance

  class EntitiesInfoImpl() extends EntitiesInfo {
    private var animals: Map[String, (AnimalBaseInfo, AnimalChromosomeInfo)] = Map.empty
    private var plants: Map[String, PlantInfo] = Map.empty


    def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit = {
      val animalChromosomeInfo = if (animals.get(id).isDefined) animals(id)._2
                                 else AnimalChromosomeInfo(Iterable.empty, Iterable.empty, Iterable.empty)
      animals += (id -> (animalBaseInfo, animalChromosomeInfo))
    }

    def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit =
      animals += (id -> (animals(id)._1, animalChromosomeInfo))

    def getAnimalInfo(id: String): Option[(AnimalBaseInfo, AnimalChromosomeInfo)] = animals.get(id)

    def setPlantInfo(id: String, plantInfo: PlantInfo): Unit = plants += (id -> plantInfo)

    def getPlantInfo(id: String): Option[PlantInfo] = plants.get(id)

  }
}






