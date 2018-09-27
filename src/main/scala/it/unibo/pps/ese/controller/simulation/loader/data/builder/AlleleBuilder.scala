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

object AlleleBuilder {

  /** Create a new empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder]]
    *
    * @return An empty [[it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder]]
    */
  def apply(): AlleleBuilder[EmptyAllele] = new AlleleBuilderImpl[EmptyAllele](None, None, None, None, None, Map())

  private class AlleleBuilderImpl[T <: AlleleStatus](val gene: Option[String],
                                  id: Option[String],
                                  dominance: Option[Double],
                                  consume: Option[Double],
                                  probability: Option[Double],
                                  effect: Map[String, Double])
                                  (implicit val status: TypeTag[T], val validStatus: TypeTag[ValidAllele])
    extends AlleleBuilder[T] with BaseGenericBuilder[T, FullAllele, PartialAlleleData, CompleteAlleleData]
      with ValidStatusGenericBuilder[T , FullAllele, PartialAlleleData, CompleteAlleleData, ValidAllele]{

    def setGene(gene: String): AlleleBuilder[T with AlleleWithGene] =
      new AlleleBuilderImpl(Some(gene), id, dominance, consume, probability, effect)

    def setId(id: String): AlleleBuilder[T with AlleleWithId] =
      new AlleleBuilderImpl(gene, Some(id), dominance, consume, probability, effect)

    def setDominance(dominance: Double): AlleleBuilder[T with AlleleWithDominance] =
      new AlleleBuilderImpl(gene, id, Some(dominance), consume, probability, effect)

    def setConsume(consume: Double): AlleleBuilder[T with AlleleWithConsume] =
      new AlleleBuilderImpl(gene, id, dominance, Some(consume), probability, effect)

    def setProbability(probability: Double): AlleleBuilder[T with AlleleWithProbability] =
      new AlleleBuilderImpl(gene, id, dominance, consume, Some(probability), effect)

    def setEffect(effect: Map[String, Double]): AlleleBuilder[T with AlleleWithEffect] =
      new AlleleBuilderImpl(gene, id, dominance, consume, probability, effect)

    def tryCompleteBuild(): Try[CompleteAlleleData] = {
      status.tpe match {
        case t if t <:< typeOf[FullAllele] =>
          val check = checkProperties()
          if(check.isEmpty) {
            Success(new AlleleDataImpl(gene, id.get, dominance, consume, probability, effect) with CompleteAlleleData)
          } else {
            Failure(check.get)
          }
        case _ =>
          Failure(CompleteBuildException("Allele " + id + " must have all fields"))
      }
    }

    def checkMandatoryProperties(): Option[CompleteBuildException] = {
      if(!id.isValid())
        Some(InvalidParamValueBuildException("Allele: " + id.get, "id", id))
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
        exception = exception ++: InvalidParamValueBuildException("Allele: " + id.get, "gene", gene)
      if(!dominance.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + id.get, "dominance", dominance)
      if(!consume.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + id.get, "consume", consume)
      if(!probability.inValidRange())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + id.get, "probability", probability)
      if(!effect.isValid())
        exception = exception ++: InvalidParamValueBuildException("Allele: " + id.get, "effect", effect)
      exception
    }

    override protected def buildPartialInstance(): PartialAlleleData = {
      new AlleleDataImpl(gene, id.get, dominance, consume, probability, effect)
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

    /**Type that defines balid allele's builder status*/
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

    override def getEffect: Option[Map[String, Double]] = _getEffect.boxToValidOption()
    override def getGene: Option[String] = _getGene.normalize()
    override def getDominance: Option[Double] = _getDominance.normalize()
    override def getConsume: Option[Double] = _getConsume.normalize()
    override def getProbability: Option[Double] = _getProbability.normalize()
  }
}