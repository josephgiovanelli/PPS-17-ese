package it.unibo.pps.ese.entitybehaviors


import it.unibo.pps.ese.genericworld.model.EntityBuilderHelpers._
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.utils.Point

import scala.util.{Failure, Random, Success}
import scala.math.floor
import scala.concurrent.ExecutionContext

case class PlantReproductionComponent(override val entitySpecifications: EntitySpecifications,
                                      geneticsSimulator: GeneticsSimulator,
                                      yearToClock: Int)
                                     (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications) {

  val MAX_SEEDS_PRODUCTION = 5
  val MAX_INSEMINATION_RADIUS = 100

  val pollinationCapacity: Double = Random.nextDouble()
  var elapsedClocksSinceLastYear: Int = floor(yearToClock / 2).toInt

  override def initialize(): Unit = {
    subscribeEvents()
  }

  private def subscribeEvents(): Unit = subscribe {
    case ComputeNextState() =>
      elapsedClocksSinceLastYear += 1
      if (elapsedClocksSinceLastYear >= yearToClock) {
        selfPollination()
        elapsedClocksSinceLastYear = 0
      }
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def selfPollination(): Unit = {
    val pollinationProbability = Random.nextDouble()
    if (pollinationProbability > pollinationCapacity) {
      val seedsGrowProbability = Random.nextDouble()
      val plantsGrown = floor(MAX_SEEDS_PRODUCTION * seedsGrowProbability).toInt
      val currentWindForce = Random.nextDouble()
      val currentInseminationRadius = floor(MAX_INSEMINATION_RADIUS * currentWindForce).toInt + plantsGrown
      if (plantsGrown > 1) {
        val result = requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest)
        result onComplete {
          case Success(data) =>
            val entities: Set[Entity] = distinctRandomPoints(plantsGrown, currentInseminationRadius, currentInseminationRadius)
              .map(point => initializeEntity(geneticsSimulator.newPlant(data.species), Point(point.x + data.position.x, point.y + data.position.y)))
/*
            publish(CreateEntities(entities))
*/
          case Failure(error) => throw error
        }
      }
    }
  }
}
