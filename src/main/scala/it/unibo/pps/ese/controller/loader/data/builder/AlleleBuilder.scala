package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.{CompleteAlleleData, PartialAlleleData}
import it.unibo.pps.ese.controller.loader.data.builder.AlleleBuilder.AlleleStatus
import it.unibo.pps.ese.controller.loader.data.builder.AlleleBuilder.AlleleStatus._

import scala.reflect.runtime.universe._

trait AlleleBuilder[T <: AlleleStatus] {
  def setGene(gene: String): AlleleBuilder[T with AlleleWithGene]
  def setId(id: String): AlleleBuilder[T with AlleleWithId]
  def setDominance(dominance: Double): AlleleBuilder[T with AlleleWithDominance]
  def setConsume(consume: Double): AlleleBuilder[T with AlleleWithConsume]
  def setProbability(probability: Double): AlleleBuilder[T with AlleleWithProbability]
  def setEffect(effect: Map[String, Double]): AlleleBuilder[T with AlleleWithEffect]
  def buildComplete(implicit ev: T =:= FullAllele): CompleteAlleleData
  def build(): PartialAlleleData
}

object AlleleBuilder {

  def apply(): AlleleBuilder[EmptyAllele] = new AlleleBuilderImpl[EmptyAllele](None, None, None, None, None, Map())

  private class AlleleBuilderImpl[T <: AlleleStatus](gene: Option[String],
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

    def buildComplete(implicit ev: T =:= FullAllele): CompleteAlleleData = {
      new AlleleDataImpl(gene, id, dominance, consume, probability, effect) with CompleteAlleleData
    }

    def build(): PartialAlleleData = {
      //require(status.tpe <:< st.tpe)
      status.tpe match {
        case t if t <:< typeOf[FullAllele] =>
          new AlleleDataImpl(gene, id, dominance, consume, probability, effect) with CompleteAlleleData
        case _ =>
          new AlleleDataImpl(gene, id, dominance, consume, probability, effect)
      }
    }
  }

  sealed trait AlleleStatus
  object AlleleStatus {
    sealed trait EmptyAllele extends AlleleStatus
    sealed trait AlleleWithId extends AlleleStatus
    sealed trait AlleleWithGene extends AlleleStatus
    sealed trait AlleleWithDominance extends AlleleStatus
    sealed trait AlleleWithConsume extends AlleleStatus
    sealed trait AlleleWithProbability extends AlleleStatus
    sealed trait AlleleWithEffect extends AlleleStatus

    type FullAllele = EmptyAllele with AlleleWithId with AlleleWithGene with AlleleWithDominance with AlleleWithConsume
      with AlleleWithProbability with AlleleWithEffect
  }

  private class AlleleDataImpl(val getGene: Option[String],
                               val getId: Option[String],
                               val getDominance: Option[Double],
                               val getConsume: Option[Double],
                               val getProbability: Option[Double],
                               _getEffect: Map[String, Double]) extends PartialAlleleData {
    override def getEffect: Option[Map[String, Double]] = if(_getEffect.nonEmpty) Some(_getEffect) else None
  }
}