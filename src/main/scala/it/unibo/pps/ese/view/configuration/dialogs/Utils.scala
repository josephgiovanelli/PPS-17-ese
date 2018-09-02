package it.unibo.pps.ese.view.configuration.dialogs




object ParseUtils {
  case class ParseOp[T](op: String => T)

  implicit val popDouble: ParseOp[Double] = ParseOp[Double](_.toDouble)
  implicit val popInt: ParseOp[Int] = ParseOp[Int](_.toInt)

  def parse[T: ParseOp](s: String): Option[T] = try { Some(implicitly[ParseOp[T]].op(s)) }
  catch {case _ => None}
}
