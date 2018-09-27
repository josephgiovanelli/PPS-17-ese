package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

/** Trait that defines services of an exception that has internally multiple motivations. Each
  * [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.Motivation]] can have sub-motivations, so
  * this trait models an exception with a hierarchy of motivations
  */
trait HierarchyMotivationsException extends Exception {
  /** Abstract type that defines return type of exception mixing operands defined in subclasses*/
  type MixedException <: HierarchyMotivationsException
  /** Exception motivations*/
  def motivations: Iterable[Motivation]
  /** Right associative operator to mix  exception with another HierarchyMotivationsException
    *
    * @param exception Exception to mix
    * @return New Exception
    */
  def +:(exception: HierarchyMotivationsException): MixedException
  /** Right associative operator to mix  exception with another HierarchyMotivationsException
    *
    * @param exception Exception to mix
    * @return New Exception
    */
  def :+(exception: HierarchyMotivationsException): MixedException
}

private[exception] abstract class AbsHierarchyMotivationsException(val motivations: Iterable[Motivation]) extends Exception(toString)
  with HierarchyMotivationsException {

  def this(motivation: String, subMotivations: Iterable[Motivation] = Seq()) {
    this(Seq(new Motivation(motivation, subMotivations)))
  }

  def this(motivation: String, exc: Iterable[HierarchyMotivationsException])(implicit i: DummyImplicit) {
    this(motivation, exc.flatMap(_.motivations))
  }
  override def toString: String = motivations.mkString("\n")
}

/** Class that represent a motivation with sub-motivations*/
class Motivation(val motivation: String, subMotivations: Iterable[Motivation]) {
  override def toString: String = {
    motivation + subMotivations.foldLeft("")((a, b) => a + "\n  " + b.toString.replaceAll("\n", "\n  "))
  }
}