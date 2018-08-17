package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.data.PlantData
import it.unibo.pps.ese.genetics.AmminoAcidUtilities.amminoAcidSeqFromString
import it.unibo.pps.ese.genetics.Utilities.seqOfElement
import Conversion._
sealed trait PlantGenerator{
  def createPlantInfoByPlantData(plantData: PlantData):PlantInfo
  def createNumberOfPlants(num:Int,plantData: PlantData):Seq[PlantInfo] ={
    seqOfElement(num,createPlantInfoByPlantData(plantData))
  }
}

object PlantGenerator extends PlantGenerator {

  def createPlantInfoByPlantData(plantData: PlantData):PlantInfo = {
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
