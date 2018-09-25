package it.unibo.pps.ese.utils

case class DefaultValue[T](get: T)
sealed trait DefaultRange[T] {
  def start: T
  def end: T
}
case class InclusiveDefaultRange[T](start: T, end: T) extends DefaultRange[T]

object DefaultValidable {
  object ValidableByDisequality {

    def checkValidity[A: DefaultValue](a: A, extraRequirements: Iterable[A => Boolean] = Iterable()): Boolean =
      implicitly[DefaultValue[A]].get != a && extraRequirements.forall(_(a))

    def checkValidityOpt[A: DefaultValue](option: Option[A], extraRequirements: Iterable[A => Boolean] = Iterable()): Boolean =
      option.isDefined && !option.contains(implicitly[DefaultValue[A]].get) && extraRequirements.forall(_(option.get))

    implicit class ValidableByDisequality[X: DefaultValue](elem: X) {
      def isValid(extraRequirements: Iterable[X => Boolean] = Iterable()): Boolean =
        checkValidity(elem, extraRequirements)
    }

    implicit class ValidableOptByDisequality[X: DefaultValue](elem: Option[X]) {
      def isValid(extraRequirements: Iterable[X => Boolean] = Iterable()): Boolean =
        checkValidityOpt(elem, extraRequirements)
    }
  }

  object ValidableInsideRange {

    def checkValidity[A: Ordering: DefaultRange](a: A, extraRequirements: Iterable[A => Boolean] = Iterable()): Boolean =
      implicitly[DefaultRange[A]] match {
        case r: InclusiveDefaultRange[A] =>
          val ordering = implicitly[Ordering[A]]
          ordering.gteq(a, r.start) && ordering.lteq(a, r.end) && extraRequirements.forall(_(a))
      }

    def checkValidityOpt[A: Ordering: DefaultRange](option: Option[A], extraRequirements: Iterable[A => Boolean] = Iterable()): Boolean =
      implicitly[DefaultRange[A]] match {
        case r: InclusiveDefaultRange[A] =>
          val ordering = implicitly[Ordering[A]]
          option.forall(a => ordering.gteq(a, r.start) && ordering.lteq(a, r.end)) && option.isDefined && extraRequirements.forall(_(option.get))
      }

    implicit class ValidableByRange[X: Ordering: DefaultRange](elem: X) {
      def inValidRange(extraRequirements: Iterable[X => Boolean] = Iterable()): Boolean = checkValidity(elem, extraRequirements)
    }
    implicit class ValidableOptByRange[X: Ordering: DefaultRange](elem: Option[X]) {
      def inValidRange(extraRequirements: Iterable[X => Boolean] = Iterable()): Boolean = checkValidityOpt(elem, extraRequirements)
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