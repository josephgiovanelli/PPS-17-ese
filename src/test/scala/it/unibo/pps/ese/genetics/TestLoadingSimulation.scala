package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.genetics.QualityType.{Fecundity, Fertility, Life, Speed}
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
    val geneticsSimulator = GeneticsSimulator
    val initializedSimulation = geneticsSimulator.beginSimulation(data)
    assert(initializedSimulation.getAllAnimals("Gatto").size == 50)
    assert(initializedSimulation.getAllPlant("ErbaGatta").size==10)

    assert(geneticsSimulator.speciesList.contains("Gatto"))
    val gatto:AnimalInfo = geneticsSimulator.newAnimal("Gatto")
    assert(gatto.species==Species(Animal,"Gatto"))
    assert(gatto.dietType==Carnivorous)
    val qualities = gatto.animalQualities

    if(gatto.gender == Male){
      assert(!(qualities contains Fertility))
      assert(!(qualities contains Fecundity))
    }else{
      assert(qualities contains Fertility)
      assert(qualities contains Fecundity)
      assert(qualities(Fertility).qualityValue==3.0)
      assert(qualities(Fecundity).qualityValue==3.0)
    }
    val speed:Double = qualities(Speed).qualityValue
    assert(speed==1.6)
    val life:Double = qualities(Life).qualityValue
    assert(life==3.0)
    assert(geneticsSimulator.mutantAlleleOfSpecies("Gatto").isEmpty)

    assert(geneticsSimulator.plantSpeciesList.contains("ErbaGatta"))
    assert(geneticsSimulator.newPlant("ErbaGatta").availability==4.0)

    assertThrows[IllegalStateException](geneticsSimulator.beginSimulation(data))
  }
}
