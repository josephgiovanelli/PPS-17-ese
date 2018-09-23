package it.unibo.pps.ese.utils

case class DefaultValue[T](get: T)

object DefaultValidable {
  object ValidableByDisequality {

    def checkValidity[A: DefaultValue](a: A): Boolean = implicitly[DefaultValue[A]].get != a

    def checkValidity[A: DefaultValue](option: Option[A]): Boolean = !option.contains(implicitly[DefaultValue[A]].get)

    implicit class ValidableByDisequality[X: DefaultValue](elem: X) {
      def isValid: Boolean = checkValidity(elem)
    }
    implicit class ValidableOptByDisequality[X: DefaultValue](elem: Option[X]) {
      def isValid: Boolean = checkValidity(elem)
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