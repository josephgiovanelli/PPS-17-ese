package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.YamlLoader
import org.scalatest.FunSuite

class TestLoadingSimulation extends FunSuite{
  test("Test loading"){
    val data = new YamlLoader().loadSimulation("it/unibo/pps/ese/controller/loader/Simulation.yml")
    val animalData:TranslatedAnimalData = InputDataAdapter.translateAnimalData(
      data.animals.keySet.toSeq.head
    )
    val speciesSetup:SpeciesSetup = new SpeciesSetup(animalData)
    val animalGenome:AnimalGenome = speciesSetup.speciesGenerator.generateAnimalGenome
    val animalFeature:AnimalFeature = speciesSetup.dnaTranslator.getQualitiesByGenome(animalGenome)
    println(animalGenome)
    println(animalFeature)
    val simulationInitializer:SimulationInitializer = SimulationInitializer(data)
    println(simulationInitializer.getAllAnimals)
    println(simulationInitializer.getAllPlant)
  }
}
