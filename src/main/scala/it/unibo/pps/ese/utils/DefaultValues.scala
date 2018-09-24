package it.unibo.pps.ese.utils

case class DefaultValue[T](get: T)
sealed trait DefaultRange[T] {
  def start: T
  def end: T
}
case class InclusiveDefaultRange[T](start: T, end: T) extends DefaultRange[T]

object DefaultValidable {
  object ValidableByDisequality {

    def checkValidity[A: DefaultValue](a: A): Boolean = implicitly[DefaultValue[A]].get != a

    def checkValidity[A: DefaultValue](option: Option[A]): Boolean = !option.contains(implicitly[DefaultValue[A]].get) && option.isDefined

    implicit class ValidableByDisequality[X: DefaultValue](elem: X) {
      def isValid: Boolean = checkValidity(elem)
    }
    implicit class ValidableOptByDisequality[X: DefaultValue](elem: Option[X]) {
      def isValid: Boolean = checkValidity(elem)
    }
  }

  object ValidableInsideRange {

    def checkValidity[A: Ordering: DefaultRange](a: A): Boolean = implicitly[DefaultRange[A]] match {
      case r: InclusiveDefaultRange[A] =>
        val ordering = implicitly[Ordering[A]]
        ordering.gteq(a, r.start) && ordering.lteq(a, r.end)
    }

    def checkValidity[A: Ordering: DefaultRange](option: Option[A]): Boolean = implicitly[DefaultRange[A]] match {
      case r: InclusiveDefaultRange[A] =>
        val ordering = implicitly[Ordering[A]]
        option.forall(a => ordering.gteq(a, r.start) && ordering.lteq(a, r.end)) && option.isDefined
    }

    implicit class ValidableByRange[X: Ordering: DefaultRange](elem: X) {
      def inValidRange: Boolean = checkValidity(elem)
    }
    implicit class ValidableOptByRange[X: Ordering: DefaultRange](elem: Option[X]) {
      def inValidRange: Boolean = checkValidity(elem)
    }
  }
}

object DefaultGet {

  def getContentOrDefault[A: DefaultValue](option: Option[A]): A = option.getOrElse(implicitly[DefaultValue[A]].get)

  implicit class DefaultGetOptional[T: DefaultValue](option: Option[T]) {
    def getOrDefault: T = {
      getContentOrDefault(option)
    }
  }
}