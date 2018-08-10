package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.beans.Plant
import it.unibo.pps.ese.controller.loader.data.PlantData

trait PlantInfo extends PlantData{
  def genome:Genome
}
object PlantInfo{
  def apply(plantData: PlantData,genome: Genome): PlantInfo = PlantInfoImpl(plantData,genome)
  case class PlantInfoImpl(plantData: PlantData,genome: Genome) extends PlantInfo{
    override def name: String = plantData.name

    override def geneLength: Int = plantData.geneLength

    override def reign: String = plantData.reign

    override def height: Double = plantData.height

    override def nutritionalValue: Double = plantData.nutritionalValue

    override def availability: Double = plantData.availability
  }
}
