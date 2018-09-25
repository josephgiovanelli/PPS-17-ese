package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.{CompleteAlleleData, PartialAlleleData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder.AlleleStatus
import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder.AlleleStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, InvalidParamValueBuildException}

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}
import it.unibo.pps.ese.utils.DefaultValidable.ValidableByDisequality._
import it.unibo.pps.ese.utils.DefaultValidable.ValidableInsideRange._

sealed trait AlleleBuilder[T <: AlleleStatus] extends GenericBuilder[T, FullAllele, PartialAlleleData, CompleteAlleleData] {
  def gene: Option[String]
  def setGene(gene: String): AlleleBuilder[T with AlleleWithGene]
  def setId(id: String): AlleleBuilder[T with AlleleWithId]
  def setDominance(dominance: Double): AlleleBuilder[T with AlleleWithDominance]
  def setConsume(consume: Double): AlleleBuilder[T with AlleleWithConsume]
  def setProbability(probability: Double): AlleleBuilder[T with AlleleWithProbability]
  def setEffect(effect: Map[String, Double]): AlleleBuilder[T with AlleleWithEffect]
}

object AlleleBuilder {

  def apply(): AlleleBuilder[EmptyAllele] = AlleleBuilder[EmptyAllele](None, None, None, None, None, Map())

  private def apply[T <: AlleleStatus: TypeTag](gene: Option[String], id: Option[String], dominance: Option[Double], consume: Option[Double], probability: Option[Double],
    effect: Map[String, Double]): AlleleBuilder[T] =
      new AlleleBuilderImpl[T](gene, id, dominance, consume, probability, effect)
        with ValidStatusGenericBuilder[T, FullAllele, PartialAlleleData, CompleteAlleleData, ValidAllele]

  private class AlleleBuilderImpl[T <: AlleleStatus](val gene: Option[String],
                                  id: Option[String],
                                  dominance: Option[Double],
                                  consume: Option[Double],
                                  probability: Option[Double],
                                  effect: Map[String, Double])
                                  (implicit val status: TypeTag[T], val validStatus: TypeTag[ValidAllele]) extends AlleleBuilder[T] {

    def setGene(gene: String): AlleleBuilder[T with AlleleWithGene] =
      AlleleBuilder(Some(gene), id, dominance, consume, probability, effect)

    def setId(id: String): AlleleBuilder[T with AlleleWithId] =
      AlleleBuilder(gene, Some(id), dominance, consume, probability, effect)

    def setDominance(dominance: Double): AlleleBuilder[T with AlleleWithDominance] =
      AlleleBuilder(gene, id, Some(dominance), consume, probability, effect)

    def setConsume(consume: Double): AlleleBuilder[T with AlleleWithConsume] =
      AlleleBuilder(gene, id, dominance, Some(consume), probability, effect)

    def setProbability(probability: Double): AlleleBuilder[T with AlleleWithProbability] =
      AlleleBuilder(gene, id, dominance, consume, Some(probability), effect)

    def setEffect(effect: Map[String, Double]): AlleleBuilder[T with AlleleWithEffect] =
      AlleleBuilder(gene, id, dominance, consume, probability, effect)

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

    def build(): PartialAlleleData = {
      tryCompleteBuild() match {
        case Success(value) =>
          value
        case Failure(_) =>
          new AlleleDataImpl(gene, id.get, dominance, consume, probability, effect)
      }
    }

    def checkMandatoryProperties(): Option[CompleteBuildException] = {
      if(!id.isValid())
        Some(InvalidParamValueBuildException("Allele: " + id.get, "id", id))
      else
        None
    }

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
  }

  sealed trait AlleleStatus extends BuilderStatus
  object AlleleStatus {
    sealed trait EmptyAllele extends AlleleStatus
    sealed trait AlleleWithId extends AlleleStatus
    sealed trait AlleleWithGene extends AlleleStatus
    sealed trait AlleleWithDominance extends AlleleStatus
    sealed trait AlleleWithConsume extends AlleleStatus
    sealed trait AlleleWithProbability extends AlleleStatus
    sealed trait AlleleWithEffect extends AlleleStatus

    type ValidAllele = EmptyAllele with AlleleWithId
    type FullAllele = ValidAllele with AlleleWithGene with AlleleWithDominance with AlleleWithConsume
      with AlleleWithProbability with AlleleWithEffect
  }

  private class AlleleDataImpl(val getGene: Option[String],
                               val id: String,
                               val getDominance: Option[Double],
                               val getConsume: Option[Double],
                               val getProbability: Option[Double],
                               _getEffect: Map[String, Double]) extends PartialAlleleData {
    override def getEffect: Option[Map[String, Double]] = if(_getEffect.nonEmpty) Some(_getEffect) else None
  }
}