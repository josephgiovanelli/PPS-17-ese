package it.unibo.pps.ese.entitybehaviors

import java.util.Random

import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.{EntityAttributesImpl => _, _}
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.support.BaseEvent
import it.unibo.pps.ese.utils.Point

case class BrainInfo(position: Point,
                     height: Double,
                     strong: Double,
                     defense: Double,
                     kind: String,
                     actionField: Double,
                     visualField: Double,
                     attractiveness: Double,
                     gender: String
                    ) extends BaseEvent
case class EntityPosition(position: Point) extends BaseEvent
case class EatEntity(entityId: String) extends BaseEvent
case class RequireDynamicParameters() extends BaseEvent
case class RequireDynamicParametersResponse(speed: Double, energy: Double, fertility: Double) extends BaseEvent

case class BrainComponent(override val entitySpecifications: EntitySpecifications,
                          heightWorld: Int,
                          widthWorld: Int,
                          var position: Point,
                          height: Double,
                          strong: Double,
                          defense: Double,
                          kind: String,
                          actionField: Double,
                          visualField: Double,
                          attractiveness: Double,
                          gender: String
                         ) extends WriterComponent(entitySpecifications)  {



  object Direction extends Enumeration {
    val RIGHT, LEFT, UP, DOWN = Value
  }

  implicit def toPoint(tuple2: (Int, Int)): Point = {
    def bound(i: Int, bound: Int): Int = if (i < 0) 0 else if (i > bound) bound else i
    def boundWidth(x: Int): Int = bound(x, widthWorld)
    def boundHeight(y: Int): Int = bound(y, heightWorld)
    Point(boundWidth(tuple2._1), boundHeight(tuple2._2))
  }


  val ENERGY_THRESHOLD = 60
  val MIN_PREYS_FOR_COUPLING = 3
  val FERTILITY_THRESHOLD = 20


  val decisionSupport: DecisionSupport = DecisionSupport()

  var entityInVisualField: Map[String, EntityAttributesImpl] = Map.empty

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = {
    import EntityInfoConversion._
    subscribe {
      case ComputeNextState() =>
        publish(RequireEntitiesState(entitySpecifications id, x => distanceBetween(x.state.position, position) <= visualField))
      case EntitiesStateResponse(id, state) if id == entitySpecifications.id =>
        entityInVisualField = Map.empty
        state map (x => EntityAttributesImpl(x.entityId, x.state.kind, x.state.height, x.state.strong, x.state.defense, (x.state.position.x, x.state.position.y), x.state.attractiveness, x.state.gender)) foreach (x => entityInVisualField += (x.name -> x))
        publish(new RequireDynamicParameters)
      case RequireDynamicParametersResponse(speed, energy, fertility) =>
        publish(EntityPosition(nextMove(speed, energy, fertility)))
        publish(new ComputeNextStateResponse)
      case GetInfo() =>
        publish(BrainInfo(position, height, strong, defense, kind, actionField, visualField, attractiveness, gender))
        publish(new GetInfoResponse)
      case _ => Unit
    }
  }

  private def configureMappings(): Unit = {
    addMapping[EntityPosition]((classOf[EntityPosition], ev => Seq(EntityProperty("position", ev position))))
    addMapping[BrainInfo]((classOf[BrainInfo], ev => Seq(
      EntityProperty("height", ev height),
      EntityProperty("position", ev position),
      EntityProperty("strong", ev strong),
      EntityProperty("defense", ev defense),
      EntityProperty("kind", ev kind),
      EntityProperty("actionField", ev actionField),
      EntityProperty("visualField", ev visualField),
      EntityProperty("attractiveness", ev attractiveness),
      EntityProperty("gender", ev gender)
    )))
  }

  private def nextMove(speed: Double, energy: Double, fertility: Double): Point = {

    val start = System.currentTimeMillis()
    val floorSpeed = speed.toInt
    val me: EntityAttributesImpl = EntityAttributesImpl(entitySpecifications id, EntityKinds(Symbol(kind)), height, strong, defense, (position.x, position.y), attractiveness, SexTypes.withNameOpt(gender).get)
    decisionSupport.createVisualField(entityInVisualField.values.toSeq :+ me)
    val partners = decisionSupport.discoverPartners(me)
    val preys = decisionSupport.discoverPreys(me)
    var targets: Stream[EntityChoiceImpl] = preys
    if (energy > ENERGY_THRESHOLD && preys.lengthCompare(MIN_PREYS_FOR_COUPLING) > 0 && fertility > FERTILITY_THRESHOLD) targets = partners
    if (targets.nonEmpty) {
      val entityChoice = targets.min(Ordering.by((_:EntityChoiceImpl).distance))
      val entityAttribute = entityInVisualField(entityChoice.name)

      if (entityChoice.distance < actionField) {
        me.position = entityAttribute.position
        publish(EatEntity(entityAttribute name))
      } else {
        (0 until floorSpeed) foreach( _ => me.position = decisionSupport.nextMove(me, entityAttribute))
      }

      position = Point(me.position.x, me.position.y)
    }
    else {
      val direction = Direction(new Random().nextInt(Direction.values.size))
      direction match {
        case Direction.DOWN => position = (position.x, position.y - floorSpeed)
        case Direction.UP => position = (position.x, position.y + floorSpeed)
        case Direction.LEFT => position = (position.x - floorSpeed, position.y)
        case Direction.RIGHT => position = (position.x + floorSpeed, position.y)
      }
    }

    val stop = System.currentTimeMillis()
    println(stop - start)

    decisionSupport.clearVisualField()
    position
  }

  private def distanceBetween(from: Point, to: Point) : Int = Math.sqrt(Math.pow(from.x - to.x, 2) + Math.pow(from.y - to.y, 2)).toInt

}