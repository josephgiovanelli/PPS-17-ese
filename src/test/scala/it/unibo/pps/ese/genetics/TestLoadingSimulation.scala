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
//    println(animalFeature)
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

    val newAnimal = geneticsSimulator.newAnimal("Gatto")
    val translateAnimalGenome = geneticsSimulator.getAnimalInfoByGenome("Gatto",newAnimal.genome)
    assert(translateAnimalGenome==newAnimal)
    var male = geneticsSimulator.newAnimal("Gatto")
    while(male.gender==Female){
      male = geneticsSimulator.newAnimal("Gatto")
    }
    var female = geneticsSimulator.newAnimal("Gatto")
    while(female.gender!=Female){
      female = geneticsSimulator.newAnimal("Gatto")
    }

    println(male)
    println(female)
    val sm=male.genome.firstGenomeSequence
    val sf=female.genome.firstGenomeSequence
    val sxcc = SexualChromosomeCouple(female.genome.firstSexualChromosome,
                                      male.genome.secondSexualChromosome)
    val childGenome = AnimalGenome(
      Map(
        ChromosomeType.COMMON -> ChromosomeCouple(sm(ChromosomeType.COMMON),sf(ChromosomeType.COMMON)),
        ChromosomeType.STRUCTURAL_ANIMAL -> ChromosomeCouple(sm(ChromosomeType.STRUCTURAL_ANIMAL),sf(ChromosomeType.STRUCTURAL_ANIMAL)),
        ChromosomeType.LIFE_CYCLE -> ChromosomeCouple(sm(ChromosomeType.LIFE_CYCLE),sf(ChromosomeType.LIFE_CYCLE)),
        ChromosomeType.FEEDING -> ChromosomeCouple(sm(ChromosomeType.FEEDING),sf(ChromosomeType.FEEDING))
      ),
      sxcc
    )
    val child = geneticsSimulator.getAnimalInfoByGenome("Gatto",childGenome)
    println(child)
  }
}
