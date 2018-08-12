package it.unibo.pps.ese.genetics

trait AnimalInfo {
  def gender:Gender
  def dietType:DietType
  def genome:AnimalGenome
  def animalQualities:Map[QualityType,Quality]
  def activeAlleles:Seq[AllelicBehaviour]
}
object AnimalInfo{
  def apply(animalFeature: AnimalFeature,genome: AnimalGenome): AnimalInfo = new AnimalInfoImpl(
    gender = animalFeature.gender,
    dietType = animalFeature.dietType,
    genome = genome,
    animalQualities = animalFeature.animalQualities,
    activeAlleles = animalFeature.activeAllelicStructure
  )
  //Anche altro apply
  case class AnimalInfoImpl(
    gender: Gender,
    dietType: DietType,
    genome: AnimalGenome,
    animalQualities:Map[QualityType,Quality],
    activeAlleles:Seq[AllelicBehaviour]
  ) extends AnimalInfo{
    override def toString: String = "Gender: "+gender+",Diet: "+dietType+", Qualities: "+animalQualities.toString()
  }
}
