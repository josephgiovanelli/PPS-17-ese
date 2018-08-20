package it.unibo.pps.ese.controller.loader.data

trait GeneData {
  def id: String
  def name: String
  def properties: Map[String, Class[_]]
  def alleles: Set[AlleleData]
}

abstract class AbsGeneData(val properties: Map[String, Class[_]], _alleles: Iterable[AlleleData]) extends GeneData {
  val alleles:  Set[AlleleData] = _alleles.toSet
  require(alleles.size == _alleles.size)
  require(alleles.nonEmpty)
  require(alleles.map(_.probability).sum == 1.0)
  alleles.foreach(
    all => {
      require(all.effect.keySet.subsetOf(properties.keySet),
        "Allele " + all.id + " of gene " + id + " (simple name: " + name + " )" +
          "tries to modify properties: " + all.effect.keySet.toString()
          + ", but the gene has only properties: "
          + properties.keySet.toString())
      require(all.gene == id, all.gene + " " + id)
    }
  )

  def canEqual(other: Any): Boolean = other.isInstanceOf[AbsGeneData]

  override def equals(other: Any): Boolean = other match {
    case that: AbsGeneData =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
