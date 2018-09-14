package it.unibo.pps.ese.utils

case class DefaultValue[T](get: T)

trait Validable[T] {
  def isValid(implicit defaultValue: DefaultValue[T]): Boolean
}

trait DefaultGet[T] {
  def getOrDefault(implicit defaultValue: DefaultValue[T]): T
}

object ValidableImplicits {
  implicit class ValidableString(str: String) extends Validable[String] {
    def isValid(implicit defaultValue: DefaultValue[String]): Boolean = str != defaultValue.get
  }

  implicit class ValidableIterable(it: Iterable[_]) extends Validable[Iterable[_]] {
    def isValid(implicit defaultValue: DefaultValue[Iterable[_]]): Boolean = it != defaultValue.get
  }

  implicit class ValidableNumeric(num: Double) extends Validable[Double] {
    def isValid(implicit defaultValue: DefaultValue[Double]): Boolean = num != defaultValue.get
  }
}

object DefaultGetImplicits {
  implicit class DefaultGetOptional[T](option: Option[T]) extends DefaultGet[T] {
    override def getOrDefault(implicit defaultValue: DefaultValue[T]): T = {
      option.getOrElse(defaultValue.get)
    }
  }
}