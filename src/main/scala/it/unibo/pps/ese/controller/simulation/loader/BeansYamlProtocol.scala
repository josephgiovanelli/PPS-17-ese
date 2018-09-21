package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.beans._
import net.jcazevedo.moultingyaml.{DefaultYamlProtocol, YamlFormat}

object BeansYamlProtocol extends DefaultYamlProtocol {
  implicit val simulationFormat: YamlFormat[Simulation] = yamlFormat2(Simulation)
  implicit val plantFormat: YamlFormat[Plant] = yamlFormat7(Plant)
  implicit val defaultChromosomeDataFormat: YamlFormat[DefaultChromosomeData] = yamlFormat2(DefaultChromosomeData)
  implicit val animalFormat: YamlFormat[Animal] = yamlFormat8(Animal)
  implicit val propertyInfoFormat: YamlFormat[PropertyInfo] = yamlFormat1(PropertyInfo)
  implicit val geneFormat: YamlFormat[Gene] = yamlFormat4(Gene)
  implicit val alleleFormat: YamlFormat[Allele] = yamlFormat6(Allele)
}
