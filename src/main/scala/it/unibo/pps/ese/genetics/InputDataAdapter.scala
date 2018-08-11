package it.unibo.pps.ese.genetics
import it.unibo.pps.ese.controller.loader.data._
import AmminoAcidUtilities._
import it.unibo.pps.ese.genetics.AllelicData.AllelicInfoImpl
import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

object InputDataAdapter {
  def translateAnimalData(aD:AnimalData):TranslatedAnimalData = {
    TranslatedAnimalDataImpl(
      aD.name,
      aD.geneLength,
      aD.reign,
      aD.typology,
      aD.structuralChromosome.map(customGeneDataToGeneData).toSeq,
      aD.regulationChromosome.map(defaultGeneDataToCustomGeneData).map(customGeneDataToGeneData).toSeq,
      aD.sexualChromosome.map(defaultGeneDataToCustomGeneData).map(customGeneDataToGeneData).toSeq
    )
  }

  private[this]def defaultGeneDataToCustomGeneData(dgd: DefaultGeneData):CustomGeneData ={
    object MyCustomGeneData extends CustomGeneData{

      override def conversionMap: Map[String, Map[String, Double]] = Map(dgd.name->Map(dgd.name->1.0))
      override def id: String = dgd.id

      override def name: String = dgd.name

      override def properties: Map[String, Class[_]] = dgd.properties

      override def alleles: Set[AlleleData] = dgd.alleles
    }
    MyCustomGeneData
  }

  def customGeneDataToGeneData(customGeneData: CustomGeneData):GeneData = {
    def getQualityTypeByString(s:String):QualityType = {
      QualityType.values.find(q=>q.entryName.toLowerCase()==s.toLowerCase).get
    }
    val geneSeq:Seq[ProteinoGenicAmminoacid] = amminoAcidSeqFromString(customGeneData.id)
    def getConversionMapSeq(map:Map[String,Double]):Seq[ConversionMap]  ={
      map.map(e=>ConversionMap(getQualityTypeByString(e._1),e._2)).toSeq
    }
    def getFeatureByName(s:String,seq: Seq[Feature]):Feature = {
      seq.find(_.name==s).get
    }
    val geneFeatures:Seq[Feature]=customGeneData.conversionMap.map(cMap=>{
      Feature(cMap._1,getConversionMapSeq(cMap._2))
    }).toSeq

    val allelicForms:Seq[AlleleInfo] = customGeneData.alleles.toSeq.map(alleleData=>{
        val featuresBehaviour:Seq[(Feature,Double)] = alleleData
                                                          .effect
                                                          .map(e=>(getFeatureByName(e._1,geneFeatures),e._2))
                                                          .toSeq
        new AllelicInfoImpl(
          geneSeq,
          amminoAcidSeqFromString(alleleData.id),
          alleleData.dominance.toInt,
          featuresBehaviour,
          alleleData.consume,
          alleleData.probability
        )
      }
    )
    GeneData(
      geneSeq,
      customGeneData.name,
      geneFeatures,
      allelicForms
    )
  }
}
