package it.unibo.pps.ese.genetics

sealed trait AnimalFeature{
  def affectQuality(qualityType: QualityType,value:Double)
  def gender_(gender: Gender)
  def gender:Gender
  def animalQualities:Map[QualityType,Quality]
  def activeAllelicStructure:Seq[AllelicBehaviour]
  def addActiveStructuralAllele(b:AllelicBehaviour)
}


class AnimalFeatureImpl extends AnimalFeature{
  var animalGender:Option[Gender] = Option.empty
  val typeFilter:((Quality,QualityType)=>Boolean) = (a,b)=>a.qualityType==b

  var qualities: Seq[Quality] = List()
  var activeStructuralBehaviour:Seq[AllelicBehaviour] = List()

  private def isQualityPresent(qualityType: QualityType):Boolean ={
    qualities.exists(typeFilter(_,qualityType))
  }

  override def affectQuality(
                              qualityType: QualityType,
                              value: Double): Unit = (isQualityPresent(qualityType),qualityType) match {

    case (true,_) => {
      val oldQualityValue:Double = qualities.find(typeFilter(_,qualityType)).get.qualityValue
      val updatedQuantity:Quality = Quality(oldQualityValue+value,qualityType)
      qualities = qualities.filter(!typeFilter(_,qualityType)) :+ updatedQuantity
    }
    case (false,_) => qualities = qualities :+ Quality(value,qualityType)
  }

  override def gender_(gender: Gender): Unit = animalGender = Some(gender)
  override def gender:Gender = animalGender.get

  override def animalQualities: Map[QualityType,Quality] = qualities.map(q =>(q.qualityType,q)).toMap
  override def toString: String = qualities.toString()

  override def activeAllelicStructure: Seq[AllelicBehaviour] = activeStructuralBehaviour

  override def addActiveStructuralAllele(b: AllelicBehaviour): Unit = activeStructuralBehaviour = activeStructuralBehaviour :+ b
}