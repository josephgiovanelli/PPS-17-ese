package it.unibo.pps.ese.utils

/** class that models a default value. Used for Type Class pattern
  *
  * @param get Default value's value
  * @tparam T Default value's type
  */
case class DefaultValue[T](get: T)

/** Trait that defines a default range. Used for Type Class pattern
  *
  * @tparam T range type
  */
sealed trait DefaultRange[T] {
  /**Range start*/
  val start: T
  /**Range end*/
  val end: T
}

/** Class that models a range with inclusive start and end
  *
  * @param start Range's start
  * @param end Range's end
  * @tparam T range type
  */
case class InclusiveDefaultRange[T](start: T, end: T) extends DefaultRange[T]

/** Object that contains functions and implicits that use type classes for data validation*/
object DefaultValidable {

  /** Object that contains functions and implicits that use type class [[it.unibo.pps.ese.utils.DefaultValue]] for data validation*/
  object ValidableByDisequality {

    /** Check validity of input argument according to implicit [[it.unibo.pps.ese.utils.DefaultValue]] defined in scope
      *
      * @param a Input argument
      * @param extraRequirements Optional collection of extra requirements
      * @tparam A Input argument's type
      * @return True if argument is valid
      */
    def checkValidity[A: DefaultValue](a: A, extraRequirements: Iterable[A => Boolean] = Iterable()): Boolean =
      implicitly[DefaultValue[A]].get != a && extraRequirements.forall(_(a))

    /** Check validity of an option that contains validable type according to implicit [[it.unibo.pps.ese.utils.DefaultValue]] defined in scope
      *
      * @param option Option that contains a validable type
      * @param extraRequirements Optional collection of extra requirements
      * @tparam A Option content's type
      * @return True if argument is valid, false if not or if option is empty
      */
    def checkValidityOpt[A: DefaultValue](option: Option[A], extraRequirements: Iterable[A => Boolean] = Iterable()): Boolean =
      option.isDefined && !option.contains(implicitly[DefaultValue[A]].get) && extraRequirements.forall(_(option.get))

    /** Implicit class to add methods to types with an [[it.unibo.pps.ese.utils.DefaultValue]] type class implicit defined*/
    implicit class ValidableByDisequality[X: DefaultValue](elem: X) {

      /** Check if element is valid according to implicit [[it.unibo.pps.ese.utils.DefaultValue]] defined in scope
        *
        * @param extraRequirements Optional extra requirements
        * @return True if is valid
        */
      def isValid(extraRequirements: (X => Boolean)*): Boolean =
        checkValidity(elem, extraRequirements)

      /** Check if element is valid according to implicit [[it.unibo.pps.ese.utils.DefaultValue]] defined in scope
        *
        * @param extraRequirements Optional extra requirements
        * @return Empty optional if object is not valid, an Option with value inside otherwise
        */
      def boxToValidOption(extraRequirements: (X => Boolean)*): Option[X] = {
        if(!isValid(extraRequirements:_*))
          None
        else
          Some(elem)
      }
    }

    /** Implicit class to add methods to options containing types with an [[it.unibo.pps.ese.utils.DefaultValue]] type class implicit defined*/
    implicit class ValidableOptByDisequality[X: DefaultValue](elem: Option[X]) {

      /** Check if Option content is valid according to implicit [[it.unibo.pps.ese.utils.DefaultValue]] defined in scope
        *
        * @param extraRequirements Optional extra requirements
        * @return True if is valid
        */
      def isValid(extraRequirements: (X => Boolean)*): Boolean =
        checkValidityOpt(elem, extraRequirements)

      /** Check if Option content is valid according to implicit [[it.unibo.pps.ese.utils.DefaultValue]] defined in scope
        *
        * @param extraRequirements Optional extra requirements
        * @return Empty optional if content is not valid, self otherwise
        */
      def normalize(extraRequirements: (X => Boolean)*): Option[X] = {
        if(!isValid(extraRequirements:_*))
          None
        else
          elem
      }
    }
  }

  /** Object that contains functions and implicits that use type class [[it.unibo.pps.ese.utils.DefaultRange]] for data validation*/
  object ValidableInsideRange {

    /** Check validity of input argument according to implicit [[it.unibo.pps.ese.utils.DefaultRange]] defined in scope
      *
      * @param a Input argument
      * @param extraRequirements Optional collection of extra requirements
      * @tparam A Input argument's type
      * @return True if argument is valid
      */
    def checkValidity[A: Ordering: DefaultRange](a: A, extraRequirements: Iterable[A => Boolean] = Iterable()): Boolean =
      implicitly[DefaultRange[A]] match {
        case r: InclusiveDefaultRange[A] =>
          val ordering = implicitly[Ordering[A]]
          ordering.gteq(a, r.start) && ordering.lteq(a, r.end) && extraRequirements.forall(_(a))
      }

    /** Check validity of an option that contains validable type according to implicit [[it.unibo.pps.ese.utils.DefaultRange]] defined in scope
      *
      * @param option Option that contains a validable type
      * @param extraRequirements Optional collection of extra requirements
      * @tparam A Option content's type
      * @return True if argument is valid, false if not or if option is empty
      */
    def checkValidityOpt[A: Ordering: DefaultRange](option: Option[A], extraRequirements: Iterable[A => Boolean] = Iterable()): Boolean =
      implicitly[DefaultRange[A]] match {
        case r: InclusiveDefaultRange[A] =>
          val ordering = implicitly[Ordering[A]]
          option.forall(a => ordering.gteq(a, r.start) && ordering.lteq(a, r.end)) && option.isDefined && extraRequirements.forall(_(option.get))
      }

    /** Implicit class to add methods to types with an [[it.unibo.pps.ese.utils.DefaultRange]] type class implicit defined*/
    implicit class ValidableByRange[X: Ordering: DefaultRange](elem: X) {

      /** Check if element is valid according to implicit [[it.unibo.pps.ese.utils.DefaultRange]] defined in scope
        *
        * @param extraRequirements Optional extra requirements
        * @return True if is valid
        */
      def inValidRange(extraRequirements: (X => Boolean)*): Boolean = checkValidity(elem, extraRequirements)

      /** Check if element is valid according to implicit [[it.unibo.pps.ese.utils.DefaultRange]] defined in scope
        *
        * @param extraRequirements Optional extra requirements
        * @return Empty optional if object is not valid, an Option with value inside otherwise
        */
      def boxToValidOption(extraRequirements: (X => Boolean)*): Option[X] = {
        if(!inValidRange(extraRequirements:_*))
          None
        else
          Some(elem)
      }
    }

    /** Implicit class to add methods to options containing types with an [[it.unibo.pps.ese.utils.DefaultRange]] type class implicit defined*/
    implicit class ValidableOptByRange[X: Ordering: DefaultRange](elem: Option[X]) {

      /** Check if Option content is valid according to implicit [[it.unibo.pps.ese.utils.DefaultRange]] defined in scope
        *
        * @param extraRequirements Optional extra requirements
        * @return True if is valid
        */
      def inValidRange(extraRequirements: (X => Boolean)*): Boolean = checkValidityOpt(elem, extraRequirements)

      /** Check if Option content is valid according to implicit [[it.unibo.pps.ese.utils.DefaultRange]] defined in scope
        *
        * @param extraRequirements Optional extra requirements
        * @return Empty optional if content is not valid, self otherwise
        */
      def normalize(extraRequirements: (X => Boolean)*): Option[X] = {
        if(!inValidRange(extraRequirements:_*))
          None
        else
          elem
      }
    }
  }
}

/** Object that contains functions that use [[it.unibo.pps.ese.utils.DefaultValue]] type class implicits to define a
  *  a default value get system to optionals
  */
object DefaultGet {

  /** Method that returns Option content or, if empty, default value defined by
    * [[it.unibo.pps.ese.utils.DefaultValue]] type class implicit defined in scope
    *
    * @param option Option
    * @tparam A Option content type
    * @return Option content or default value if empty
    */
  def getContentOrDefault[A: DefaultValue](option: Option[A]): A = option.getOrElse(implicitly[DefaultValue[A]].get)

  /** Implict class to add default get method to options*/
  implicit class DefaultGetOptional[T: DefaultValue](option: Option[T]) {

    /** Returns Option content or, if empty, default value defined by
      * [[it.unibo.pps.ese.utils.DefaultValue]] type class implicit defined in scope
      *
      * @return Option content or default value if empty
      */
    def getOrDefault: T = {
      getContentOrDefault(option)
    }
  }
}