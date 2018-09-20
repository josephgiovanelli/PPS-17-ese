package it.unibo.pps.ese.model.components.animals.brain

import java.util.Random

import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.MemoryType.MemoryType
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus.Hippocampus
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus.Hippocampus.SearchingState
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.MemoryType
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.support.{BaseEvent, RequestEvent, ResponseEvent, SupervisedFuture}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.components._
import it.unibo.pps.ese.model.components.animals.DigestionEnd
import it.unibo.pps.ese.model.components.animals.brain.ActionKind.ActionKind
import it.unibo.pps.ese.model.components.animals.brain.Direction.Direction
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.{EntityAttributesImpl => _, _}
import it.unibo.pps.ese.model.components.animals.reproduction._
import it.unibo.pps.ese.utils.{Point, Position}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

case class BrainInfo(strength: Double,
                     actionField: Double,
                     visualField: Double,
                     attractiveness: Double
                    ) extends BaseEvent

object ActionKind extends Enumeration {
  type ActionKind = Value
  val EAT, COUPLE, NOTHING = Value
}

case class UseHippocampus() extends BaseEvent
case class UseEyes() extends BaseEvent
case class EntityWill(will: ActionKind.Value) extends BaseEvent

case class EntityPosition(position: Point) extends BaseEvent
case class InteractionEntity(entityId: String, action: ActionKind.Value) extends BaseEvent
case class DynamicParametersRequest() extends RequestEvent
case class DynamicParametersResponse(override val id: String, speed: Double, energy: Double, fertility: Double, satisfaction: Double) extends ResponseEvent

object Direction extends Enumeration {
  type Direction = Value
  val RIGHT, LEFT, UP, DOWN, NONE = Value
}

case class BrainComponent(override val entitySpecifications: EntitySpecifications,
                          heightWorld: Int,
                          widthWorld: Int,
                          strength: Double,
                          actionField: Double,
                          visualField: Double,
                          attractiveness: Double)
                         (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications)  {


  implicit def toPoint(tuple2: (Int, Int)): Point = {
    def bound(i: Int, bound: Int): Int = if (i < 0) 0 else if (i > bound) bound else i
    def boundWidth(x: Int): Int = bound(x, widthWorld)
    def boundHeight(y: Int): Int = bound(y, heightWorld)
    Point(boundWidth(tuple2._1), boundHeight(tuple2._2))
  }

  implicit def actionKindToMemoryType(actionKind: ActionKind): MemoryType = actionKind match {
    case ActionKind.EAT => MemoryType.HUNTING
    case ActionKind.COUPLE => MemoryType.COUPLE
  }

  implicit def pointToPosition(point: Point): Position = {
    Position(point.x, point.y)
  }


  val ENERGY_THRESHOLD = 60
  val MIN_PREYS_FOR_COUPLING = 2
  val FERTILITY_THRESHOLD = 0.4
  val SATISFACTION_THRESHOLD = 40

  var digestionState: Boolean = false

  var pregnantState: Boolean = false

  var forceReproduction: Option[ForceReproduction] = None

  val decisionSupport: DecisionSupport = DecisionSupport()

  val hippocampus: Hippocampus = Hippocampus(widthWorld, heightWorld, visualField)

  var entityInVisualField: Map[String, EntityAttributesImpl] = Map.empty

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

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
      case p: Pregnant =>
        pregnantState = true
      case p: PregnancyEnd =>
        pregnantState = false
      case GetInfo() =>
        publish(BrainInfo(strength, actionField, visualField, attractiveness))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  private def configureMappings(): Unit = {
    addMapping[EntityPosition]((classOf[EntityPosition], ev => Seq(EntityProperty("position", ev position))))
    addMapping[ComputeNextState]((classOf[ComputeNextState], _ => Seq(EntityProperty("will", ActionKind.NOTHING))))
    addMapping[EntityWill]((classOf[EntityWill], ev => Seq(EntityProperty("will", ev will))))
    addMapping[BrainInfo]((classOf[BrainInfo], ev => Seq(
      EntityProperty("strength", ev strength),
      EntityProperty("actionField", ev actionField),
      EntityProperty("visualField", ev visualField),
      EntityProperty("attractiveness", ev attractiveness),
      EntityProperty("will", ActionKind.NOTHING)
    )))
  }

  private def nextMove(speed: Double, energy: Double, fertility: Double, satisfaction: Double): SupervisedFuture[Point] = {

    requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest) map (data => {
      var position = data position
      val floorSpeed = speed.toInt
      val me: EntityAttributesImpl = AnimalAttributes(entitySpecifications id,
        EntityKinds(Symbol(data species)), data height, strength, data defense, (data.position.x, data.position.y),
        attractiveness, SexTypes.withNameOpt(data gender).get)
      decisionSupport.createVisualField(entityInVisualField.values.toSeq :+ me)
      val partners = decisionSupport.discoverPartners(me)
      val preys = decisionSupport.discoverPreys(me)
      var targets: Stream[EntityChoiceImpl] = preys
      var action: ActionKind.Value = ActionKind.EAT
      if (energy > ENERGY_THRESHOLD
        && preys.lengthCompare(MIN_PREYS_FOR_COUPLING) > 0
        && fertility > FERTILITY_THRESHOLD
        && satisfaction < SATISFACTION_THRESHOLD
        && !pregnantState) {
        targets = partners; action = ActionKind.COUPLE
      }
      if (action.equals(ActionKind.COUPLE) || (action.equals(ActionKind.EAT) && !digestionState)) {
        publish(EntityWill(action))
        if (targets.nonEmpty) {
          publish(UseEyes())
          val entityChoice = targets.min(Ordering.by((_:EntityChoiceImpl).distance))
          val entityAttribute = entityInVisualField(entityChoice.name)

          if (entityChoice.distance < actionField) {
            me.position = entityAttribute.position
            publish(InteractionEntity(entityAttribute name, action))
            hippocampus.notifyEvent(action, Position(me.position.x, me.position.y))
            if (action.equals(ActionKind.EAT)) digestionState = true
          } else {
            (0 until floorSpeed) foreach( _ => me.position = decisionSupport.nextMove(me, entityAttribute))
          }

          position = Point(me.position.x, me.position.y)
        }
        else {
          position = hippocampus.searchingState match {
            case SearchingState.INACTIVE =>
              hippocampus.startNewSearch(action)
              publish(UseHippocampus())
              checkNewMemory
            case SearchingState.ACTIVE =>
              publish(UseHippocampus())
              val d = hippocampus.computeDirection(position)
              val p = getPosition(d)
              p
            case SearchingState.ENDED => getPosition(randomDirection)
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