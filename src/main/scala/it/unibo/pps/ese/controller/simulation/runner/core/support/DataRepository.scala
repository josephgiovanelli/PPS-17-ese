package it.unibo.pps.ese.controller.simulation.runner.core.support

import scala.collection.mutable

sealed trait DataRepository[A, B] {

  def exists(id : A) : Boolean
  def addOrUpdate(id : A, element: B) : Unit
  def getById(id : A) : Option[B]
  def deleteById(id : A) : Unit
  def getAll: Seq[(A, B)]
  def deleteAll() : Unit

}

object DataRepository {

  def apply[A, B]: DataRepository[A, B] =  InMemoryRepository[A, B]()

  private case class InMemoryRepository[A, B]() extends DataRepository[A, B] {

    private val _data : mutable.HashMap[A, B] = new mutable.HashMap[A, B]()

    def exists(id : A) : Boolean = _data contains id
    override def addOrUpdate(id : A, element: B): Unit = _data(id) = element
    override def getById(id: A): Option[B] = _data get id
    override def deleteById(id: A): Unit = _data -= id
    override def getAll: Seq[(A, B)] = _data toSeq
    override def deleteAll(): Unit = _data clear

  }
}
