package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.FakeEvent._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.{DecisionSupport, EntityAttributesImpl, EntityChoiceImpl, EntityKinds}


case class BrainComponent(fakeBus: FakeBus, var position: FakePoint) extends FakeComponent(fakeBus: FakeBus) {

  val decisionSupport: DecisionSupport = DecisionSupport()

  var name: Option[Int] = None
  var height: Option[Int] = None
  var strong: Option[Int] = None
  var defense: Option[Int] = None
  var kind: Option[String] = None

  var speed: Option[Int] = None
  var visualField: Option[Int] = None
  var actionField: Option[Int] = None

  var entityInVisualField: Map[Int, EntityAttributesImpl] = Map.empty

  override def consume(fakeEvent: FakeEvent): Unit = fakeEvent match {
    case FakeEvent(a, b) if a.equals(FakeEventType.PING) => println("pong")
    case FakeEvent(a, b) if a.equals(FakeEventType.NAME) => name = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.HEIGHT) => height = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.STRONG) => strong = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.DEFENSE) => defense = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.KIND) => kind = Some(b)
    case FakeEvent(a, b) if a.equals(FakeEventType.SPEED) => speed = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.VISUAL_FIELD) => visualField = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.ACTION_FIELD) => actionField = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.CALCULATE_NEXT_MOVE) => fakeBus.publish(FakeEvent(FakeEventType.NEXT_MOVE, nextMove.toString))
    case _ => Unit
  }

  private def nextMove: FakePoint = {
    entityInVisualField = Map.empty
    FakeBuffer.instance().getEntityInVisualField(name.get) map
      (x => mapToEntityattributes(x)) foreach(x => entityInVisualField += (x.name -> x))
    val me: EntityAttributesImpl = EntityAttributesImpl(name.get, EntityKinds(Symbol(kind.get)), height.get, strong.get, defense.get, (position.x, position.y))
    decisionSupport.createVisualField(entityInVisualField.values.toSeq :+ me)
    val entityChoice = decisionSupport.discoverPreys(me).min(Ordering.by((_:EntityChoiceImpl).distance))
    (0 until speed.get) foreach( _ => me.position = decisionSupport.nextMove(me, entityInVisualField(entityChoice.name)))
    decisionSupport.clearVisualField()
    position = FakePoint(me.position.x, me.position.y)
    position
  }

  private def mapToEntityattributes(representation: FakeEntityRepresentation): EntityAttributesImpl =
    EntityAttributesImpl(representation.name, EntityKinds(Symbol(representation.kind)), representation.height, representation.strong, representation.defense, (representation.point.x, representation.point.y))

}
