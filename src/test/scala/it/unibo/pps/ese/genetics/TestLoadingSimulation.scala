package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.YamlLoader
import org.scalatest.FunSuite

class TestLoadingSimulation extends FunSuite{
  test("Test loading"){
    val animals = new YamlLoader().loadSimulation("it/unibo/pps/ese/controller/loader/Simulation.yml").animals
    val animalData:TranslatedAnimalData = InputDataAdapter.translateAnimalData(
      animals.keySet.toSeq.head
    )
    val speciesSetup:SpeciesSetup = new SpeciesSetup(animalData)
    val animalGenome:AnimalGenome = speciesSetup.speciesGenerator.generateAnimalGenome
    val animalFeature:AnimalFeature = speciesSetup.dnaTranslator.getQualitiesByGenome(animalGenome)
    println(animalGenome)
    println(animalFeature)
  }
}
