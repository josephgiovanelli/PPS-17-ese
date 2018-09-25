package it.unibo.pps.ese.controller.simulation.loader.data

trait GeneData[A <: PartialAlleleData] {
  val name: String
  def getId: Option[String]
  def getProperties: Option[Map[String, Class[_]]]
  def getAlleles: Option[Iterable[A]]

  def canEqual(other: Any): Boolean = other.isInstanceOf[GeneData[_]]

  override def equals(other: Any): Boolean = other match {
    case that: GeneData[_] =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

trait FullGeneData[A <: PartialAlleleData] extends GeneData[A] {
  def id: String = getId.getOrElse(throw new IllegalStateException())
  def properties: Map[String, Class[_]] = getProperties.getOrElse(throw new IllegalStateException())
  def alleles: Set[A] = getAlleles.getOrElse(throw new IllegalStateException()).toSet
}

object GeneData {
  type PartialGeneData = GeneData[_ <: PartialAlleleData]
  trait CompleteGeneData extends FullGeneData[CompleteAlleleData]
}
//  val alleles:  Set[AlleleData] = _alleles.toSet
//  //TODO
////  require(alleles.size == _alleles.size)
////  alleles.foreach(
////    all => {
////      require(all.effect.keySet.subsetOf(properties.keySet),
////        "Allele " + all.id + " of gene " + id + " (simple name: " + name + " )" +
////          "tries to modify properties: " + all.effect.keySet.toString()
////          + ", but the gene has only properties: "
////          + properties.keySet.toString())
////      require(all.gene == id, all.gene + " " + id)
////    }
////  )

