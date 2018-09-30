package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.{CompleteAlleleData, PartialAlleleData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder.AlleleStatus
import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder.AlleleStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._
import it.unibo.pps.ese.utils.DefaultValidable.ValidableInsideRange._

/** Builder that can build a PartialAlleleData as partial data instance and a CompleteAlleleData as complete data
  * instance
  *
  * @tparam T Builder's current status
  */
sealed trait AlleleBuilder[T <: AlleleStatus] extends GenericBuilder[T, FullAllele, PartialAlleleData, CompleteAlleleData] {
  /**
    * @return Current allele's gene
    */
  def gene: Option[String]

  /** Set allele's gene
    *
    * @param gene Allele's gene
    * @return New builder with updated param and status
    */
  def setGene(gene: String): AlleleBuilder[T with AlleleWithGene]

  /** Set allele's id
    *
    * @param id Allele's id
    * @return New builder with updated param and status
    */
  def setId(id: String): AlleleBuilder[T with AlleleWithId]

  /** Set allele's dominance level
    *
    * @param dominance Allele's dominance
    * @return New builder with updated param and status
    */
  def setDominance(dominance: Double): AlleleBuilder[T with AlleleWithDominance]

  /** Set allele's energetic consume
    *
    * @param consume Allele's consume
    * @return New builder with updated param and status
    */
  def setConsume(consume: Double): AlleleBuilder[T with AlleleWithConsume]

  /** Set allele's probability
    *
    * @param probability Allele's probability
    * @return New builder with updated param and status
    */
  def setProbability(probability: Double): AlleleBuilder[T with AlleleWithProbability]

  /** Set allele's effect represented by map containing effected qualities' name as key and numeric effect
    * as value
    *
    * @param effect Allele's effect
    * @return New builder with updated param and status
    */
  def setEffect(effect: Map[String, Double]): AlleleBuilder[T with AlleleWithEffect]
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder]]*/
object AlleleBuilder {

  /** Create a new empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder]]
    *
    * @return An empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder]]
    */
  def apply(): AlleleBuilder[EmptyAllele] = new AlleleBuilderImpl[EmptyAllele](None, None, None, None, None, Map())

  private class AlleleBuilderImpl[T <: AlleleStatus](val gene: Option[String],
                                                     _id: Option[String],
                                                     _dominance: Option[Double],
                                                     _consume: Option[Double],
                                                     _probability: Option[Double],
                                                     _effect: Map[String, Double])
                                  (implicit val status: TypeTag[T], val validStatus: TypeTag[ValidAllele])
    extends AlleleBuilder[T] with BaseGenericBuilder[T, FullAllele, PartialAlleleData, CompleteAlleleData]
      with ValidStatusGenericBuilder[T , FullAllele, PartialAlleleData, CompleteAlleleData, ValidAllele]{

    def setGene(gene: String): AlleleBuilder[T with AlleleWithGene] =
      new AlleleBuilderImpl(Some(gene), _id, _dominance, _consume, _probability, _effect)

    def setId(id: String): AlleleBuilder[T with AlleleWithId] =
      new AlleleBuilderImpl(gene, Some(id), _dominance, _consume, _probability, _effect)

    def setDominance(dominance: Double): AlleleBuilder[T with AlleleWithDominance] =
      new AlleleBuilderImpl(gene, _id, Some(dominance), _consume, _probability, _effect)

    def setConsume(consume: Double): AlleleBuilder[T with AlleleWithConsume] =
      new AlleleBuilderImpl(gene, _id, _dominance, Some(consume), _probability, _effect)

    def setProbability(probability: Double): AlleleBuilder[T with AlleleWithProbability] =
      new AlleleBuilderImpl(gene, _id, _dominance, _consume, Some(probability), _effect)

    def setEffect(effect: Map[String, Double]): AlleleBuilder[T with AlleleWithEffect] =
      new AlleleBuilderImpl(gene, _id, _dominance, _consume, _probability, effect)

    def tryCompleteBuild(): Try[CompleteAlleleData] = {
      status.tpe match {
        case t if t <:< typeOf[FullAllele] =>
          val check = checkProperties()
          if(check.isEmpty) {
            Success(new AlleleDataImpl(gene, _id.get, _dominance, _consume, _probability, _effect) with CompleteAlleleData)
          } else {
            Failure(check.get)
          }
        case _ =>
          Failure(CompleteBuildException("Allele " + _id + " must have all fields"))
      }
    }

    def checkMandatoryProperties(): Option[CompleteBuildException] = {
      if(!_id.isValid())
        Some(InvalidParamValueBuildException("Allele: " + _id.get, "id", _id))
      else
        None
    }

    /** Check builder status depending by fields value
      *
      * @return Optional exception occurred during checks
      */
    def checkProperties(): Option[CompleteBuildException] = {
      var exception: Option[CompleteBuildException] = checkMandatoryProperties()
      if(!gene.isValid())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + _id.get, "gene", gene)
      if(!_dominance.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + _id.get, "dominance", _dominance)
      if(!_consume.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + _id.get, "consume", _consume)
      if(!_probability.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + _id.get, "probability", _probability)
      if(!_effect.isValid())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + _id.get, "effect", _effect)
      exception
    }

    override protected def buildPartialInstance(): PartialAlleleData = {
      new AlleleDataImpl(gene, _id.get, _dominance, _consume, _probability, _effect)
    }
  }

  /** Interface that represent generic allele's builder status*/
  sealed trait AlleleStatus extends BuilderStatus
  /** Object containing all possible allele's builder statuses*/
  object AlleleStatus {
    sealed trait EmptyAllele extends AlleleStatus
    sealed trait AlleleWithId extends AlleleStatus
    sealed trait AlleleWithGene extends AlleleStatus
    sealed trait AlleleWithDominance extends AlleleStatus
    sealed trait AlleleWithConsume extends AlleleStatus
    sealed trait AlleleWithProbability extends AlleleStatus
    sealed trait AlleleWithEffect extends AlleleStatus

    /**Type that defines valid allele's builder status*/
    type ValidAllele = EmptyAllele with AlleleWithId
    /**Type that defines complete allele's builder status*/
    type FullAllele = ValidAllele with AlleleWithGene with AlleleWithDominance with AlleleWithConsume
      with AlleleWithProbability with AlleleWithEffect
  }

  private[this] class AlleleDataImpl(_getGene: Option[String],
                                     val id: String,
                                     _getDominance: Option[Double],
                                     _getConsume: Option[Double],
                                     _getProbability: Option[Double],
                                     _getEffect: Map[String, Double]) extends PartialAlleleData {
    import BuildersValidationImplicits._

    override val getEffect: Option[Map[String, Double]] = _getEffect.boxToValidOption()
    override val getGene: Option[String] = _getGene.normalize()
    override val getDominance: Option[Double] = _getDominance.normalize()
    override val getConsume: Option[Double] = _getConsume.normalize()
    override val getProbability: Option[Double] = _getProbability.normalize()
  }
}