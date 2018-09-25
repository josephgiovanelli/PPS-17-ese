package it.unibo.pps.ese.model.components.plant

import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityBuilderHelpers._
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.model.components._
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.utils.Point

import scala.concurrent.ExecutionContext
import scala.math.floor
import scala.util.{Failure, Random, Success}

case class PlantReproductionComponent(override val entitySpecifications: EntitySpecifications,
                                      geneticsSimulator: GeneticsSimulator,
                                      yearToClock: Int)
                                     (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications) {

  val MAX_SEEDS_PRODUCTION = 4
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
            import it.unibo.pps.ese.controller.simulation.runner.core.support.RandomHelper._
            val entities: Set[Entity] = Random.distinctRandomPoints(plantsGrown, currentInseminationRadius, currentInseminationRadius)
              .map(point => initializeEntity(geneticsSimulator.newPlant(data.species), Point(point.x + data.position.x, point.y + data.position.y)))
            publish(Create(entities))
          case Failure(error) => throw error
        }
      }
    }
  }
}
