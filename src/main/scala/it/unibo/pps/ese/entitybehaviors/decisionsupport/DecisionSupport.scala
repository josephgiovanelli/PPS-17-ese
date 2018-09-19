package it.unibo.pps.ese.entitybehaviors.decisionsupport


import it.unibo.pps.ese.entitybehaviors.StaticRules

import scala.math._

trait DecisionSupport extends WorldTypesImpl {
  val worldRules: WorldRules = StaticRules.instance().getRules()

  implicit def tupleToEntityChoice(tuple: (Name, Int)): EntityChoice = new EntityChoice(tuple._1, tuple._2)
  implicit def streamTupleToStreamEntityChoice(tuples: Stream[(Name, Int)]): Stream[EntityChoice] = tuples map tupleToEntityChoice

  def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit
  def clearVisualField(): Unit

  def discoverPreys(hunter: EntityAttributes): Stream[EntityChoice]
  def discoverPartners(entity: EntityAttributes): Stream[EntityChoice]
  def nextMove(from: EntityAttributes, to: EntityAttributes): GeneralPosition[PositionMeasure]

}



object DecisionSupport {
  def apply(): DecisionSupport = new DecisionSupportImpl()

  private class DecisionSupportImpl() extends DecisionSupport {

    private var visualField: Set[EntityAttributes] = Set.empty

    override def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit = visualField ++= entitiesAttributes

    override def clearVisualField(): Unit = visualField = Set.empty

    override def discoverPreys(hunter: EntityAttributes): Stream[EntityChoice] =
      basicFilter(hunter) filter (prey => compatiblePreysKind(hunter, prey)) filter (prey => simulateAttack(hunter, prey)) map (prey => calculateLength(hunter, prey))

    override def discoverPartners(hunter: EntityAttributes): Stream[EntityChoice] =
      basicFilter(hunter) filter (partner => compatiblePartnersKind(hunter, partner)) filter (partner => simulateCoupling(hunter, partner)) map (partner => calculateLength(hunter, partner))

    override def nextMove(from: EntityAttributes, to: EntityAttributes): GeneralPosition[PositionMeasure] = (from.position, to.position) match  {
      case (f, t) if (f sameAbscissa t) > 0 => (f.x - 1, f.y)
      case (f, t) if (f sameAbscissa t) < 0 => (f.x + 1, f.y)
      case (f, t) if (f sameOrdinate t) > 0 => (f.x, f.y - 1)
      case (f, t) if (f sameOrdinate t) < 0 => (f.x, f.y + 1)
      case (f, _) => f
    }

    private def basicFilter(entity: EntityAttributes): Stream[EntityAttributes] = {
//      println()
//      println(visualField.filter(e => e.kind == EntityKinds(Symbol("Gatto"))).forall(e => e.sex == SexTypes.male ))
//      println(visualField.filter(e => e.kind == EntityKinds(Symbol("Gatto"))).forall(e => e.sex == SexTypes.female ))
//      println(visualField.filter(e => e.kind == EntityKinds(Symbol("Giraffa"))).forall(e => e.sex == SexTypes.male ))
//      println(visualField.filter(e => e.kind == EntityKinds(Symbol("Giraffa"))).forall(e => e.sex == SexTypes.female ))
      visualField filter (prey => prey != entity) filter (other => heightDiff(entity, other)) toStream
    }

    private def heightDiff(entity1: EntityAttributes, entity2: EntityAttributes): Boolean = {
      val diff = abs(entity1.height - entity2.height)
      diff < worldRules.heightThresholds
    }

    private def compatiblePreysKind(hunter: EntityAttributes, prey: EntityAttributes): Boolean =
      worldRules.compatibleHuntingKinds.contains((hunter.kind, prey.kind))

    private def compatiblePartnersKind(hunter: EntityAttributes, partner: EntityAttributes): Boolean =
      worldRules.compatibleCouplingKinds.contains((hunter.kind, partner.kind))

    private def simulateAttack(hunter: EntityAttributes, prey: EntityAttributes): Boolean =
      (hunter.strong - prey.defense) > worldRules.attackThreshold

    private def simulateCoupling(entity: EntityAttributes, partner: EntityAttributes): Boolean = entity.sex == SexTypes.male && partner.sex == SexTypes.female &&
      partner.attractiveness > worldRules.couplingThreshold && (simulateAttack(entity, partner) || entity.attractiveness > worldRules.couplingThreshold)

    private def calculateLength(hunter: EntityAttributes, prey: EntityAttributes): EntityChoice =
      (prey.name, abs(hunter.position.x - prey.position.x) + abs(hunter.position.y - prey.position.y))

  }

}

