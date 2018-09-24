package it.unibo.pps.ese.model.components.animals.brain

import java.util.Random

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.{BaseEvent, RequestEvent, ResponseEvent}
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.MemoryType
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.data.{EntityProperty, EntityState}
import it.unibo.pps.ese.controller.simulation.runner.core.support.SupervisedFuture
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.components._
import it.unibo.pps.ese.model.components.animals.DigestionEnd
import it.unibo.pps.ese.model.components.animals.brain.Direction.Direction
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport._
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityAttributesImplUtils._
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus.Hippocampus
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus.Hippocampus._
import it.unibo.pps.ese.model.components.animals.reproduction._
import it.unibo.pps.ese.utils.{Point, Position}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * Event with which inform the rest of the world of BrainComponent state.
  * @param strength the strength quality of the entity in subject
  * @param actionField the action field of the entity in subject
  * @param visualField the visual field of the entity in subject
  * @param attractiveness the attractiveness quality of the entity in subject
  */
case class BrainInfo(strength: Double,
                     actionField: Double,
                     visualField: Double,
                     attractiveness: Double) extends BaseEvent

/**
  * The enumeration that describes the type of possible actions.
  */
trait ActionTypes
case object Eat extends ActionTypes
case object Couple extends ActionTypes
case object Nothing extends ActionTypes

/**
  * It informs that the entity is using the hippocampus.
  */
case class UseHippocampus() extends BaseEvent

/**
  * It informs that the entity saw something that interests him.
  */
case class UseEyes() extends BaseEvent

/**
  * It informs about why the entity are moving.
  * (move because is searching for [[Eat]] or [[Couple]])
  * @param will the reason
  */
case class EntityWill(will: ActionTypes) extends BaseEvent

/**
  * It informs about the entity position.
  * @param position the entity position
  */
case class EntityPosition(position: Point) extends BaseEvent

/**
  * It informs that the entity is doing an action.
  * @param entityId the entity identifier
  * @param action the action
  */
case class InteractionEntity(entityId: String, action: ActionTypes) extends BaseEvent

/**
  * It informs that the brain requires its dynamic parameters.
  */
case class DynamicParametersRequest() extends RequestEvent

/**
  * This is the response of [[DynamicParametersRequest]] with the dynamic parameters.
  * @param id entity identifier
  * @param speed current speed
  * @param energy current energy
  * @param fertility current fertility
  * @param satisfaction current satisfaction
  */
case class DynamicParametersResponse(override val id: String, speed: Double, energy: Double, fertility: Double, satisfaction: Double) extends ResponseEvent

/**
  * Enumeration that defines the direction that the entity can take.
  */
object Direction extends Enumeration {
  type Direction = Value
  val RIGHT, LEFT, UP, DOWN, NONE = Value
}

/**
  * The component that is responsible for the decisions made (movements, hunting and finding partners)
  * @param entitySpecifications the base information of the entity
  * @param heightWorld the height of the world
  * @param widthWorld the width of the world
  * @param strength the strenght of the entity
  * @param actionField the action field of the entity
  * @param visualField the visual field of the entity
  * @param attractiveness the attractiveness of the entity
  * @param executionContext the execution context
  */
case class BrainComponent(override val entitySpecifications: EntitySpecifications,
                          heightWorld: Int,
                          widthWorld: Int,
                          strength: Double,
                          actionField: Double,
                          visualField: Double,
                          attractiveness: Double)
                         (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications)  {

  /**
    * This function makes sure that any position that circulates in the world is within its limits.
    * The control is done automatically, implicitly, and in this way also converts a tuple to a point, so as to write a point in a simpler way.
    * @param tuple2 the tuple to convert and check
    * @return the point in the bounds
    */
  implicit def toPoint(tuple2: (Int, Int)): Point = {
    def bound(i: Int, bound: Int): Int = if (i < 0) 0 else if (i > bound) bound else i
    def boundWidth(x: Int): Int = bound(x, widthWorld)
    def boundHeight(y: Int): Int = bound(y, heightWorld)
    Point(boundWidth(tuple2._1), boundHeight(tuple2._2))
  }

  implicit def actionKindToMemoryType(actionKind: ActionTypes): MemoryType = actionKind match {
    case Eat => cerebralcortex.Hunting
    case Couple => cerebralcortex.Couple
  }

  implicit def pointToPosition(point: Point): Position = {
    Position(point.x, point.y)
  }

  /**
    * Minimum threshold of energy to ensure that an entity can go in search of partners instead of worrying about hunting a prey.
    */
  private val ENERGY_THRESHOLD = 60

  /**
    * Minimum of prey that must have an entity around it for can go in search of partners instead of worrying about hunting a prey.
    */
  private val MIN_PREYS_FOR_COUPLING = 2

  /**
    * Minimum threshold that must have an entity to be able to mate
    */
  private val FERTILITY_THRESHOLD = 0.4

  /**
    * Maximum threshold that must have an entity to be able to mate, otherwise it is satisfied.
    */
  private val SATISFACTION_THRESHOLD = 40

  /**
    * It takes into account whether it is in digestion or not.
    */
  private var digestionState: Boolean = false

  /**
    * It takes into account whether it is in pregnant state or not.
    */
  private var pregnantState: Boolean = false

  /**
    * A message to make sure that once the partner has been reached, the reproduction takes place.
    */
  private var forceReproduction: Option[ForceReproduction] = None

  /**
    * The element that allows to catalog the entities in radius and find out if there are prey or partners.
    */
  private val decisionSupport: DecisionSupport = DecisionSupport()

  private val hippocampus: Hippocampus = Hippocampus(widthWorld, heightWorld, visualField)

  /**
    * It stores the entities in its radius of vision.
    */
  private var entityInVisualField: Map[String, EntityAttributesImpl] = Map.empty

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  /**
    * Method by which events are received and reacted.
    */
  private def subscribeEvents(): Unit = {
    import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
    subscribe {
      case ComputeNextState() =>
        if(forceReproduction.isDefined) {
          publish(ForceReproductionForward(forceReproduction.get))
          forceReproduction = None
          publish(new ComputeNextStateAck)
        } else {
          hippocampus.updateTime()

          val data = for {
            dynamicData <- requireData[DynamicParametersRequest, DynamicParametersResponse](new DynamicParametersRequest)
            baseInfo <- requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest)
            external <- requireData[EntitiesStateRequest, EntitiesStateResponse](EntitiesStateRequest(x => distanceBetween(x.state.position, baseInfo position) <= visualField))
          } yield (external, dynamicData)

          data onComplete{
            case Success((extData, dynData)) =>
              entityInVisualField = Map.empty
              extData.state map (x => convertToEntityAttributes(x)) foreach (x => entityInVisualField += (x.name -> x))

              nextMove(dynData speed, dynData energy, dynData fertility, dynData satisfaction) onComplete (r => {
                publish(EntityPosition(r.get))
                publish(new ComputeNextStateAck)
              })
            case Failure(error) => throw error
          }

          def convertToEntityAttributes(x: EntityState): EntityAttributesImpl = if (x.state.reign == ReignType.ANIMAL) AnimalAttributes(x.entityId, x.state.species, x.state.height,
            x.state.strength, x.state.defense, (x.state.position.x, x.state.position.y),
            x.state.attractiveness, x.state.gender) else PlantAttributes(x.entityId, x.state.species, x.state.height, x.state.defense, (x.state.position.x, x.state.position.y), x.state.gender)
        }
      case r: AutoForceReproduction =>
        forceReproduction = Some(r)
      case r: PartnerForceReproduction if r.receiverId == entitySpecifications.id =>
        forceReproduction = Some(r)
      case DigestionEnd() =>
        digestionState = false
      case _: Pregnant =>
        pregnantState = true
      case _: PregnancyEnd =>
        pregnantState = false
      case GetInfo() =>
        publish(BrainInfo(strength, actionField, visualField, attractiveness))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  private def configureMappings(): Unit = {
    addMapping[EntityPosition]((classOf[EntityPosition], ev => Seq(EntityProperty("position", ev position))))
    addMapping[ComputeNextState]((classOf[ComputeNextState], _ => Seq(EntityProperty("will", Nothing))))
    addMapping[EntityWill]((classOf[EntityWill], ev => Seq(EntityProperty("will", ev will))))
    addMapping[BrainInfo]((classOf[BrainInfo], ev => Seq(
      EntityProperty("strength", ev strength),
      EntityProperty("actionField", ev actionField),
      EntityProperty("visualField", ev visualField),
      EntityProperty("attractiveness", ev attractiveness),
      EntityProperty("will", Nothing)
    )))
  }

  private def nextMove(speed: Double, energy: Double, fertility: Double, satisfaction: Double): SupervisedFuture[Point] = {

    requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest) map (data => {
      var position = data position
      val floorSpeed = speed.toInt
      val me: EntityAttributesImpl = AnimalAttributes(entitySpecifications id,
        EntityKinds(Symbol(data species)), data height, strength, data defense, (data.position.x, data.position.y),
        attractiveness, GenderTypes.withNameOpt(data gender).get)
      decisionSupport.createVisualField(entityInVisualField.values.toSeq :+ me)
      val partners = decisionSupport.discoverPartners(me)
      val preys = decisionSupport.discoverPreys(me)
      var targets: Stream[EntityChoiceImpl] = preys
      var action: ActionTypes = Eat
      if (energy > ENERGY_THRESHOLD
        && preys.lengthCompare(MIN_PREYS_FOR_COUPLING) > 0
        && fertility > FERTILITY_THRESHOLD
        && satisfaction < SATISFACTION_THRESHOLD
        && !pregnantState) {
        targets = partners; action = Couple
      }
      if (action.equals(Couple) || (action.equals(Eat) && !digestionState)) {
        publish(EntityWill(action))
        if (targets.nonEmpty) {
          publish(UseEyes())
          val entityChoice = targets.min(Ordering.by((_:EntityChoiceImpl).distance))
          val entityAttribute = entityInVisualField(entityChoice.name)

          if (entityChoice.distance < actionField) {
            me.position = entityAttribute.position
            publish(InteractionEntity(entityAttribute name, action))
            hippocampus.notifyEvent(action, Position(me.position.x, me.position.y))
            if (action.equals(Eat)) digestionState = true
          } else {
            (0 until floorSpeed) foreach( _ => me.position = decisionSupport.nextMove(me, entityAttribute))
          }

          position = Point(me.position.x, me.position.y)
        }
        else {
          position = hippocampus.searchingState match {
            case Inactive =>
              hippocampus.startNewSearch(action)
              publish(UseHippocampus())
              checkNewMemory
            case Active =>
              publish(UseHippocampus())
              val d = hippocampus.computeDirection(position)
              val p = getPosition(d)
              p
            case Ended => getPosition(randomDirection)
          }

          def checkNewMemory: Point = {
            if (hippocampus.hasNewMemory) {
              hippocampus.chooseNewMemory(position)
              val d = hippocampus.computeDirection(position)
              val p = getPosition(d)
              p
            } else getPosition(randomDirection)
          }

          def randomDirection: Direction = {
            Direction(new Random().nextInt(Direction.values.size-1))
          }


          def getPosition(direction: Direction): Point = direction match {
            case Direction.UP => (position.x, position.y - floorSpeed)
            case Direction.DOWN => (position.x, position.y + floorSpeed)
            case Direction.LEFT => (position.x - floorSpeed, position.y)
            case Direction.RIGHT => (position.x + floorSpeed, position.y)
            case Direction.NONE => checkNewMemory
          }
        }
      }
      decisionSupport.clearVisualField()
      position
    })
  }

  private def distanceBetween(from: Point, to: Point) : Int = Math.sqrt(Math.pow(from.x - to.x, 2) + Math.pow(from.y - to.y, 2)).toInt

}