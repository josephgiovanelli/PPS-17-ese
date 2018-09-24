package it.unibo.pps.ese.model.genetics.generators

import it.unibo.pps.ese.controller.simulation.loader.data.CompletePlantData
import it.unibo.pps.ese.model.genetics.Utilities.seqOfElement
import it.unibo.pps.ese.model.genetics.dna._
import it.unibo.pps.ese.model.genetics.entities.PlantInfo
/**
  To generate some plants
 */
sealed trait PlantGenerator{
  /**
    * Create a single plant starting from [[CompletePlantData]]
    * @param plantData
    * @return
    */
  def createPlantInfoByPlantData(plantData: CompletePlantData):PlantInfo
  def createNumberOfPlants(num:Int,plantData: CompletePlantData):Seq[PlantInfo] =List.range(0,num).map(_=>createPlantInfoByPlantData(plantData))
}

object PlantGenerator extends PlantGenerator {

  def createPlantInfoByPlantData(plantData: CompletePlantData):PlantInfo = {
    import NametoGeneUtilities._
    val commonGenes = List(stringToReignGene(plantData.reign),speciesNameToGene(plantData.name))
    val cc1 = Chromosome(ChromosomeType.COMMON,commonGenes :_*)
    val cc2 = Chromosome(ChromosomeType.COMMON,commonGenes :_*)
    val ccc = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }

    ccc.addChromosomeCouple(cc1,cc2)
    val structuralGenes:Seq[BasicGene] = allPropertiesGene("height","nutritionValue","Availability")
    val sc1 = Chromosome(ChromosomeType.STRUCTURAL_PLANT,structuralGenes :_*)
    val sc2 = Chromosome(ChromosomeType.STRUCTURAL_PLANT,structuralGenes :_*)
    val scc = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    scc.addChromosomeCouple(sc1,sc2)
    val genome:Genome = PlantGenome(Map(
        ChromosomeType.COMMON -> ccc,
        ChromosomeType.STRUCTURAL_PLANT -> scc
      )
    )
    PlantInfo(plantData,genome)
  }

  private[this] object NametoGeneUtilities{
    def speciesNameToGene(s:String):BasicGene = {
      BasicGene(amminoAcidSeqFromString(s),IdentifierGene)
    }
    def propertyNameToGene(s:String):BasicGene = {
      BasicGene(amminoAcidSeqFromString(s),StructuralGene)
    }
    def allPropertiesGene(seq:String*):Seq[BasicGene] = {
      seq.map(propertyNameToGene)
    }
  }

}
