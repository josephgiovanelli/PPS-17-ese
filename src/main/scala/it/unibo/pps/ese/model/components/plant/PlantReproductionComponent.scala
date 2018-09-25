package it.unibo.pps.ese.model.components.plant

import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityBuilderHelpers._
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.model.components._
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.utils.Point

import scala.concurrent.ExecutionContext
import scala.math.floor
import scala.util.{Failure, Random, Success}

/**
  * Component that manage the reproduction of the plants.
  * @param entitySpecifications the base information of the entity
  * @param geneticsSimulator the instance of genetics simulator
  * @param yearToClock the conversion of the years in terms of clock
  * @param executionContext the execution context
  */
case class PlantReproductionComponent(override val entitySpecifications: EntitySpecifications,
                                      geneticsSimulator: GeneticsSimulator,
                                      yearToClock: Int)
                                     (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications) {

  /**
    * The maximum of seeds that a plant can produce.
    */
  val MAX_SEEDS_PRODUCTION = 4

  /**
    * The maximum distance that a plant can grow away from the parent plant, considering all the favorable situations.
    */
  val MAX_INSEMINATION_RADIUS = 100

  /**
    * Ability to self-pollinate.
    */
  val pollinationCapacity: Double = Random.nextDouble()

  /**
    * Simply count to schedule the time.
    */
  var elapsedClocksSinceLastYear: Int = floor(yearToClock / 2).toInt

  override def initialize(): Unit = {
    subscribeEvents()
  }

  /**
    * Method by which events are received and the brain specifies the reaction to them.
    */
  private def subscribeEvents(): Unit = subscribe {
    /*
    If a year has passed, the plant tries to pollinate itself
    */
    case ComputeNextState() =>
      elapsedClocksSinceLastYear += 1
      if (elapsedClocksSinceLastYear >= yearToClock) {
        selfPollination()
        elapsedClocksSinceLastYear = 0
      }
      publish(new ComputeNextStateAck)
    /*
    If this message is received the physical status has to communicate his information.
     */
    case GetInfo() =>
      publish(new GetInfoAck)
    case _ => Unit
  }

  /**
    * Attempt at self-pollination.
    */
  private def selfPollination(): Unit = {
    val pollinationProbability = Random.nextDouble()
    //Every year it is not deterministic that the plant is able to pollinate, so a random number is generated
    if (pollinationCapacity > pollinationProbability) {
      //Every year it is not deterministic that the plant is able to plant all the seeds, so a random number is generated
      val seedsGrowProbability = Random.nextDouble()
      val plantsGrown = floor(MAX_SEEDS_PRODUCTION * seedsGrowProbability).toInt
      //The wind is simulated in that position, so a random number is generated
      val currentWindForce = Random.nextDouble()
      val currentInseminationRadius = floor(MAX_INSEMINATION_RADIUS * currentWindForce).toInt + plantsGrown
      if (plantsGrown > 1) {
        val result = requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest)
        result onComplete {
          //If everything has been successful, reproduction is simulated.
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
