package it.unibo.pps.ese.utils

case class DefaultValue[T](get: T)

trait Validable[T] {
  def isValid(implicit defaultValue: DefaultValue[T]): Boolean
}

trait DefaultGet[T] {
  def getOrDefault(implicit defaultValue: DefaultValue[T]): T
}

object ValidableImplicits {
  object ValidableByDisequality {
    implicit class ValidableByDisequality[X](elem: X) {
      def isValid(implicit defaultValue: DefaultValue[X]): Boolean = elem != defaultValue.get
    }
    implicit class ValidableOptByDisequality[X](elem: Option[X]) {
      def isValid(implicit defaultValue: DefaultValue[X]): Boolean = !elem.contains(defaultValue.get)
    }
  }
}

object DefaultGetImplicits {
  implicit class DefaultGetOptional[T](option: Option[T]) extends DefaultGet[T] {
    override def getOrDefault(implicit defaultValue: DefaultValue[T]): T = {
      option.getOrElse(defaultValue.get)
    }
  }
}