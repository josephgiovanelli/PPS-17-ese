package it.unibo.pps.ese.entitybehaviors

import java.util.Random

import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.{DecisionSupport, EntityAttributesImpl, EntityChoiceImpl, EntityKinds}
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.support.BaseEvent
import it.unibo.pps.ese.utils.Point

case class BrainInfo(position: Point,
                     height: Int,
                     strong: Int,
                     defense: Int,
                     kind: String,
                     actionField: Int,
                     visualField: Int) extends BaseEvent
case class EntityPosition(position: Point) extends BaseEvent
case class EatEntity(entityId: String) extends BaseEvent
case class RequireSpeed() extends BaseEvent
case class RequireSpeedResponse(speed: Int) extends BaseEvent

case class BrainComponent(override val entitySpecifications: EntitySpecifications,
                          var position: Point,
                          height: Int,
                          strong: Int,
                          defense: Int,
                          kind: String,
                          actionField: Int,
                          visualField: Int
                         ) extends WriterComponent(entitySpecifications)  {

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
        state map (x => EntityAttributesImpl(x.entityId, x.state.kind, x.state.height, x.state.strong, x.state.defense, (x.state.position.x, x.state.position.y))) foreach (x => entityInVisualField += (x.name -> x))
        publish(new RequireSpeed)
      case RequireSpeedResponse(speed) =>
        publish(EntityPosition(nextMove(speed)))
        publish(new ComputeNextStateResponse)
      case GetInfo() =>
        publish(BrainInfo(position, height, strong, defense, kind, actionField, visualField))
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
      EntityProperty("visualField", ev visualField)
    )))
  }

  private def nextMove(speed: Int): Point = {

    val start = System.currentTimeMillis()

    val me: EntityAttributesImpl = EntityAttributesImpl(entitySpecifications id, EntityKinds(Symbol(kind)), height, strong, defense, (position.x, position.y))
    decisionSupport.createVisualField(entityInVisualField.values.toSeq :+ me)
    val preys = decisionSupport.discoverPreys(me)
    if (preys.nonEmpty) {
      val entityChoice = preys.min(Ordering.by((_:EntityChoiceImpl).distance))
      val entityAttribute = entityInVisualField(entityChoice.name)

      if (entityChoice.distance < actionField) {
        me.position = entityAttribute.position
        publish(EatEntity(entityAttribute name))
      } else {
        (0 until speed) foreach( _ => me.position = decisionSupport.nextMove(me, entityAttribute))
      }

      position = Point(me.position.x, me.position.y)
    }
    else {
      if (new Random().nextBoolean()) position = Point(position.x + speed, position.y)
      else position = Point(position.x, position.y + speed)
    }

    val stop = System.currentTimeMillis()
    println(stop - start)

//    if (new Random().nextBoolean()) position = Point(position.x + speed, position.y)
//          else position = Point(position.x, position.y + speed)
    decisionSupport.clearVisualField()
    position
  }

  private def distanceBetween(from: Point, to: Point) : Int = Math.sqrt(Math.pow(from.x - to.x, 2) + Math.pow(from.y - to.y, 2)).toInt

}