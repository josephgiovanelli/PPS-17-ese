package it.unibo.pps.ese.entitybehaviors.decisionsupport


import it.unibo.pps.ese.entitybehaviors.decisionsupport.Point.Point

import scala.math._

trait DecisionSupport {
  implicit def tupleToEntityChoice(tuple: (Int, Int)): EntityChoice = EntityChoice(tuple._1, tuple._2)
  implicit def streamTupleToStreamEntityChoice(tuples: Stream[(Int, Int)]): Stream[EntityChoice] = tuples map tupleToEntityChoice

  def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit
  def clearVisualField(): Unit
  def discoverPreys(hunter: EntityAttributes): Stream[EntityChoice]
  def discoverPartners(entity: EntityAttributes): Stream[EntityChoice]
  def nextMove(from: EntityAttributes, to: EntityAttributes): Point

}

object DecisionSupport {
  def apply(): DecisionSupport = new DecisionSupportImpl()

  private class DecisionSupportImpl() extends DecisionSupport {

    private var visualField: Set[EntityAttributes] = Set.empty

    override def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit = visualField ++= entitiesAttributes

    override def clearVisualField(): Unit = visualField = Set.empty

    override def discoverPreys(hunter: EntityAttributes): Stream[EntityChoice] =
      basicFilter(hunter) filter (prey => compatiblePreysKind(hunter, prey)) filter (prey => simulateAttack(hunter, prey)) map (prey => calculateLength(hunter, prey))

    override def discoverPartners(entity: EntityAttributes): Stream[EntityChoice] =
      basicFilter(entity) filter (partner => compatiblePartnersKind(entity, partner)) filter (partner => simulateCoupling(entity, partner)) map (partner => calculateLength(entity, partner))

    override def nextMove(from: EntityAttributes, to: EntityAttributes): Point = (from.position, to.position) match  {
      case (f, t) if (f sameAbscissa t) > 0 => (f.x - 1, f.y)
      case (f, t) if (f sameAbscissa t) < 0 => (f.x + 1, f.y)
      case (f, t) if (f sameOrdinate t) > 0 => (f.x, f.y - 1)
      case (f, t) if (f sameOrdinate t) < 0 => (f.x, f.y + 1)
    }

    private def basicFilter(entity: EntityAttributes): Stream[EntityAttributes] =
      visualField filter (prey => !prey.equals(entity)) filter (other => heightDiff(entity, other)) toStream

    private def heightDiff(entity1: EntityAttributes, entity2: EntityAttributes): Boolean = {
      val diff = abs(entity1.height - entity2.height)
      diff > 0 && diff < 5
    }

    private def compatiblePreysKind(hunter: EntityAttributes, prey: EntityAttributes): Boolean =
      (hunter.kind == EntityKinds.carnivorous && prey.kind == EntityKinds.herbivore) ||
        (hunter.kind == EntityKinds.herbivore && prey.kind == EntityKinds.plant)

    private def compatiblePartnersKind(hunter: EntityAttributes, partner: EntityAttributes): Boolean =
      (hunter.kind != EntityKinds.plant) && (hunter.kind equals partner.kind)

    private def simulateAttack(hunter: EntityAttributes, prey: EntityAttributes): Boolean = (hunter.strong - prey.defense) > 3

    private def simulateCoupling(entity: EntityAttributes, partner: EntityAttributes): Boolean = ???

    private def calculateLength(hunter: EntityAttributes, prey: EntityAttributes): (Int, Int) =
      (prey.name, abs(hunter.position._1 - prey.position._1) + abs(hunter.position._2 - prey.position._2))
  }

}

