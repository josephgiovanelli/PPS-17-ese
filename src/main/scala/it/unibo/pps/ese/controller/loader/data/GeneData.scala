package it.unibo.pps.ese.controller.loader.data

trait PartialGeneData {
  def getId: Option[String]
  def getName: Option[String]
  def getProperties: Option[Map[String, Class[_]]]
  def getAlleles: Option[Set[AlleleData]]
}
trait CompleteGeneData extends PartialGeneData {
  def id: String = getId.getOrElse(throw new IllegalStateException())
  def name: String = getName.getOrElse(throw new IllegalStateException())
  def properties: Map[String, Class[_]] = getProperties.getOrElse(throw new IllegalStateException())
  def alleles: Set[AlleleData] = getAlleles.getOrElse(throw new IllegalStateException())
}

//abstract class AbsGeneData(properties: Map[String, Class[_]], _alleles: Iterable[AlleleData]) extends PartialGeneData {
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
//
//  //TODO
////  def canEqual(other: Any): Boolean = other.isInstanceOf[AbsGeneData]
////
////  override def equals(other: Any): Boolean = other match {
////    case that: AbsGeneData =>
////      (that canEqual this) &&
////        id == that.id
////    case _ => false
////  }
////
////  override def hashCode(): Int = {
////    val state = Seq(id)
////    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
////  }
//}
