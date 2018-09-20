package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.util.io.File
import it.unibo.pps.ese.genetics.dna.{AnimalGenome, BasicGene, ChromosomeType, GeneWithAllelicForms, MGene}
import it.unibo.pps.ese.genetics.entities.QualityType.{Fecundity, Fertility, Life, Speed}
import org.scalatest.FunSuite
import it.unibo.pps.ese.genetics.entities._
import it.unibo.pps.ese.genetics.generators.SpeciesUtilities
import it.unibo.pps.ese.genetics.generators.data.{InputDataAdapter, TranslatedAnimalData}
import org.kaikikm.threadresloader.ResourceLoader
class TestLoadingSimulation extends FunSuite{
  test("Test loading"){
    val data = YamlLoader.loadSimulation(File(ResourceLoader.getResource("it/unibo/pps/ese/controller/loader/Simulation.yml"))).asInstanceOf[CompleteSimulationData]
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
    val qualities = gatto.qualities

    if(gatto.gender == Male){
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
//    assert(geneticsSimulator.obtainMutantAlleles("Gatto").isEmpty)

    assert(geneticsSimulator.plantSpeciesList.contains("ErbaGatta"))

//    assertThrows[IllegalStateException](geneticsSimulator.beginSimulation(data))

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
    val sxcc = female.genome.firstSexualChromosome:+:male.genome.secondSexualChromosome

    def printGeneType(g:MGene):Unit = g match {
      case BasicGene(g,t)=> println(g)
      case GeneWithAllelicForms(g,a,t)=>println(g+"-Al-"+a)
    }

    sm.values.foreach(c=> {
      println(c.chromosomeType)
      c.geneList.foreach(printGeneType)
    })
    val childGenome = Map(
        sm(ChromosomeType.COMMON)|->|sf(ChromosomeType.COMMON),
        sm(ChromosomeType.STRUCTURAL_ANIMAL)|->| sf(ChromosomeType.STRUCTURAL_ANIMAL),
        sm(ChromosomeType.LIFE_CYCLE)|->|sf(ChromosomeType.LIFE_CYCLE),
        sm(ChromosomeType.FEEDING) |->| sf(ChromosomeType.FEEDING)
      )|%-%| sxcc

    val child = geneticsSimulator.getAnimalInfoByGenome("Gatto",childGenome)
    println(child)
    assert(geneticsSimulator.obtainMutantAlleles("Gatto",
      sm(ChromosomeType.STRUCTURAL_ANIMAL).geneList.head).nonEmpty)
    println(female.qualities.values.size)
    var males = 0
    var females = 0
    for(i<-1 to 1000) {
      geneticsSimulator.newAnimal("Gatto").gender match {
        case Male => males +=1
        case Female =>  females +=1
      }
    }
    println(initializedSimulation.getAllAnimals("Gatto").groupBy(a=> a.gender).map(c=>c._1+",size: "+c._2.size))
    println(males,females)
  }
}
