package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.controller.loader.DefaultGene
import it.unibo.pps.ese.controller.loader.data.AlleleData

case class AnimalBaseInfo(geneLength: Int, alleleLength: Int, reign: String, typology: String)
case class AnimalChromosomeInfo(var structuralChromosome: Map[String, (CustomGeneInfo, Map[String, AlleleData])], var regulationChromosome: Map[String, (DefaultGeneInfo, Map[String, AlleleData])],
                                var sexualChromosome: Map[String, (DefaultGeneInfo, Map[String, AlleleData])])
case class PlantInfo(height: Double, nutritionalValue: Double, hardness: Double, availability: Double)


class GeneInfo(val id: String, val name: String, val properties: Map[String, Class[_]])
case class DefaultGeneInfo(defaultGene: DefaultGene, override val id: String)
  extends GeneInfo(id, defaultGene.name, defaultGene.properties)
case class CustomGeneInfo(override val id: String, override val name: String,override val  properties: Map[String, Class[_]], conversionMap: Map[String, Map[String, Double]]) extends GeneInfo(id, name, properties)
case class ConversionMap(property: String, map: Map[String, Double])
sealed trait EntitiesInfo {

  def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit

  def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit

  def getAnimalInfo(id: String): Option[(AnimalBaseInfo, AnimalChromosomeInfo)]

  def setPlantInfo(id: String, plantInfo: PlantInfo): Unit

  def getPlantInfo(id: String): Option[PlantInfo]
}

object ChromosomeTypes extends Enumeration {
  val STRUCTURAL, REGULATION, SEXUAL = Value
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
                                 else AnimalChromosomeInfo(Map.empty, Map.empty, Map.empty)
      animals += (id -> (animalBaseInfo, animalChromosomeInfo))
    }

    def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit =
      animals += (id -> (animals(id)._1, animalChromosomeInfo))

    def getAnimalInfo(id: String): Option[(AnimalBaseInfo, AnimalChromosomeInfo)] = animals.get(id)

    def setPlantInfo(id: String, plantInfo: PlantInfo): Unit = plants += (id -> plantInfo)

    def getPlantInfo(id: String): Option[PlantInfo] = plants.get(id)

    def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes.Value, customGeneInfo: CustomGeneInfo): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalInfo(id) match {
        case Some((_, chromosomeInfo)) => chromosomeInfo
        case None => throw new IllegalStateException()
      }
      val currentStructuralChromosome = currentAnimalChromosome.structuralChromosome
      val alleles: Map[String, AlleleData] = if (currentStructuralChromosome.get(customGeneInfo.name).isDefined) currentStructuralChromosome(customGeneInfo.name)._2 else Map()
      val tuple = (customGeneInfo, alleles)
      currentAnimalChromosome.structuralChromosome += (customGeneInfo.name -> tuple)
    }

    def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes.Value, defaultGeneInfo: DefaultGeneInfo): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalInfo(id) match {
        case Some((_, chromosomeInfo)) => chromosomeInfo
        case None => throw new IllegalStateException()
      }
      var currentDefaultChromosome = chromosomeTypes match {
        case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome
        case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome
      }
      val alleles: Map[String, AlleleData] = if (currentDefaultChromosome.get(defaultGeneInfo.name).isDefined) currentDefaultChromosome(defaultGeneInfo.name)._2 else Map()
      currentDefaultChromosome += (defaultGeneInfo.name -> (defaultGeneInfo, alleles))
      chromosomeTypes match {
        case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome = currentDefaultChromosome
        case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome = currentDefaultChromosome
      }
    }

    def setChromosomeAlleles(id: String, chromosomeTypes: ChromosomeTypes.Value, gene: String, alleles: Map[String, AlleleData]): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(id) match {
        case Some((_, chromosomeInfo)) => chromosomeInfo
        case None => throw new IllegalStateException()
      }
      chromosomeTypes match {
        case ChromosomeTypes.STRUCTURAL => {
          val structuralGene = currentAnimalChromosome.structuralChromosome(gene)
          currentAnimalChromosome.structuralChromosome += (gene -> (structuralGene._1, structuralGene._2 ++ alleles))
        }
        case ChromosomeTypes.REGULATION => {
          val regulationGene = currentAnimalChromosome.regulationChromosome(gene)
          currentAnimalChromosome.regulationChromosome += (gene -> (regulationGene._1, regulationGene._2 ++ alleles))
        }
        case ChromosomeTypes.SEXUAL => {
          val sexualGene = currentAnimalChromosome.sexualChromosome(gene)
          currentAnimalChromosome.sexualChromosome += (gene -> (sexualGene._1, sexualGene._2 ++ alleles))
        }
      }
    }

  }
}






