package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.{CompleteAlleleData, PartialAlleleData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder.AlleleStatus
import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder.AlleleStatus._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

sealed trait AbsAlleleBuilder[T <: AlleleStatus] {
  def gene: Option[String]
  def setGene(gene: String): AlleleBuilder[T with AlleleWithGene]
  def setId(id: String): AlleleBuilder[T with AlleleWithId]
  def setDominance(dominance: Double): AlleleBuilder[T with AlleleWithDominance]
  def setConsume(consume: Double): AlleleBuilder[T with AlleleWithConsume]
  def setProbability(probability: Double): AlleleBuilder[T with AlleleWithProbability]
  def setEffect(effect: Map[String, Double]): AlleleBuilder[T with AlleleWithEffect]
}

trait AlleleBuilder[T <: AlleleStatus] extends AbsAlleleBuilder[T] with GenericBuilder[T, FullAllele, PartialAlleleData, CompleteAlleleData]

object AlleleBuilder {

  def apply(): AlleleBuilder[EmptyAllele] = new AlleleBuilderImpl[EmptyAllele](None, None, None, None, None, Map())

  private class AlleleBuilderImpl[T <: AlleleStatus](val gene: Option[String],
                                  id: Option[String],
                                  dominance: Option[Double],
                                  consume: Option[Double],
                                  probability: Option[Double],
                                  effect: Map[String, Double])
                                  (implicit val status: TypeTag[T]) extends AlleleBuilder[T]{

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

    def buildComplete(implicit ev: T =:= FullAllele, st: TypeTag[T]): CompleteAlleleData = {
      new AlleleDataImpl(gene, id.get, dominance, consume, probability, effect) with CompleteAlleleData
    }

    def tryCompleteBuild(): Try[CompleteAlleleData] = {
      status.tpe match {
        case t if t <:< typeOf[FullAllele] =>
          Success(new AlleleDataImpl(gene, id.get, dominance, consume, probability, effect) with CompleteAlleleData)
        case _ =>
          Failure(new CompleteBuildException("Allele " + id + " must have all fields"))
      }
    }

    def build(): PartialAlleleData = {
      require(status.tpe <:< typeOf[ValidAllele])
      tryCompleteBuild() match {
        case Success(value) =>
          value
        case Failure(_) =>
          new AlleleDataImpl(gene, id.get, dominance, consume, probability, effect)
      }
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