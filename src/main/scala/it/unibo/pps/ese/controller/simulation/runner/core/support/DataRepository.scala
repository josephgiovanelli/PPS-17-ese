package it.unibo.pps.ese.controller.simulation.runner.core.support

import scala.collection.mutable

/**
  * Basic key-value data storage
  * @tparam A Key type
  * @tparam B Value type
  */
sealed trait DataRepository[A, B] {

  /**
    * Check key existence
    * @param id The key
    * @return True if key is present, false otherwise
    */
  def exists(id : A) : Boolean

  /**
    * Update the data repository values
    * @param id The key to whom values to update are associated
    * @param element The updated value
    */
  def addOrUpdate(id : A, element: B) : Unit

  /**
    * Get data from the repository if exist
    * @param id The key to whom data to get are associated
    * @return
    */
  def getById(id : A) : Option[B]

  /**
    * Delete data in the repository if exist
    * @param id The key to whom data to delete are associated
    */
  def deleteById(id : A) : Unit

  /**
    * Get all the data contained in the repository
    * @return The requested data
    */
  def getAll: Seq[(A, B)]

  /**
    * Clear the repository
    */
  def deleteAll() : Unit

}

object DataRepository {

  def apply[A, B]: DataRepository[A, B] =  new InMemoryRepository[A, B]()

  /**
    * Implementation based on a mutable HashMap
    * @tparam A Key type
    * @tparam B Value type
    */
  private class InMemoryRepository[A, B]() extends DataRepository[A, B] {

    private val _data : mutable.HashMap[A, B] = new mutable.HashMap[A, B]()

    def exists(id : A) : Boolean = _data contains id
    override def addOrUpdate(id : A, element: B): Unit = _data(id) = element
    override def getById(id: A): Option[B] = _data get id
    override def deleteById(id: A): Unit = _data -= id
    override def getAll: Seq[(A, B)] = _data toSeq
    override def deleteAll(): Unit = _data clear

  }
}
