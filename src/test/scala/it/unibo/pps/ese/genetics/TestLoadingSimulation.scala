package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.YamlLoader
import org.scalatest.FunSuite

class TestLoadingSimulation extends FunSuite{
  test("Test loading"){
    val data = new YamlLoader().loadSimulation("it/unibo/pps/ese/controller/loader/Simulation.yml")
    val animalData:TranslatedAnimalData = InputDataAdapter.translateAnimalData(
      data.animals.keySet.toSeq.head
    )
    val speciesSetup:SpeciesUtilities = SpeciesUtilities(animalData)
    val animalGenome:AnimalGenome = speciesSetup.generateAnimalGenome
    val animalFeature:AnimalInfo = speciesSetup.translateGenome(animalGenome)
    println(animalGenome)
    println(animalFeature)
    val simulationInitializer:SimulationInitializer = SimulationInitializer(data)
    println(simulationInitializer.getAllAnimals)
    println(simulationInitializer.getAllPlant)
  }
}
