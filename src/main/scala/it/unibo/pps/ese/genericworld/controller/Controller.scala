package it.unibo.pps.ese.genericworld.controller

import java.util.concurrent.atomic.AtomicLong

import it.unibo.pps.ese.dataminer.{DataMiner, ReadOnlyEntityRepository}
import it.unibo.pps.ese.entitywatchers.{Stalker, StoryTeller, Surgeon}
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.view.View

import scala.concurrent.ExecutionContext

sealed trait Controller {
  def attachView(view: View, frameRate: Int): Unit
  def manage: ManageableController
}

trait ManageableController {
  def play(): Unit
  def pause(): Unit
  def exit(): Unit
  def entityData(id: String): Option[EntityState]
  def watch(entity: String): Unit
  def unwatch(): Unit
  def add(entitySpecies: Map[String, Int])
}

object Controller {

  def apply(simulation: SimulationLoop, realTimeState: ReadOnlyEntityState, consolidatedState: ReadOnlyEntityRepository)
           (implicit executionContext: ExecutionContext): Controller =
    BaseController(simulation, realTimeState, consolidatedState)

  private case class BaseController(simulation: SimulationLoop,
                                    realTimeState: ReadOnlyEntityState,
                                    consolidatedState: ReadOnlyEntityRepository)
                                   (implicit executionContext: ExecutionContext) extends Controller with ManageableController {

    private[this] var _stop = false
    private[this] var _paused = true
    private[this] val _era: AtomicLong = new AtomicLong(1)
    private[this] val surgeon = Surgeon(realTimeState)
    private[this] val stalker = Stalker(consolidatedState)
    private[this] val storyTeller = StoryTeller(consolidatedState)


    //ASYNC CALLBACK
    consolidatedState attachNewDataListener(era => {
      println("Era " + era + " data ready (Population trend: " + DataMiner(consolidatedState).populationTrend() + ")")

      //storyTeller updateHistoryLog era
      //stalker.informAboutTrueEra(era)

      //val a = DataMiner(consolidatedState).bornCount(_era get())
      //val b = DataMiner(consolidatedState).bornCount("Giraffa")
      //val c = DataMiner(consolidatedState).bornCount("Giraffa", _era get())
      //val d = DataMiner(consolidatedState).bornCount

      //val a1 = DataMiner(consolidatedState).deadCount(_era get())
      //val b1 = DataMiner(consolidatedState).deadCount("Giraffa")
      //val c1 = DataMiner(consolidatedState).deadCount("Giraffa", _era get())
      //val d1 = DataMiner(consolidatedState).deadCount

      //val a2 = DataMiner(consolidatedState).aliveSpecies(_era get())
      //val a3 = DataMiner(consolidatedState).extinctSpecies(_era get())
      //val a4 = DataMiner(consolidatedState).extinctSpecies()
      //val a5 = DataMiner(consolidatedState).simulationSpecies()
      //      if (_era == 10) {
      //        val tmp = (DataAggregator ingestedData) entitiesInEra  1
      //        tmp filter (x => x.structuralData.reign == "ANIMAL") take 1 foreach (x => {
      //          val y = (DataAggregator ingestedData) entityDynamicLog  x.id
      //          //println(y)
      //
      //          val originalData = (DataAggregator ingestedData) getAllDynamicLogs()
      //          val saver = DataSaver()
      //          val serialized = saver saveData("", originalData)
      //          val deserialized = saver loadData serialized
      //          println(deserialized)
      //        })
      //      }
    })

    simulation attachEraListener(era => _era set era)

    override def attachView(view: View, frameRate: Int): Unit = {
      storyTeller attachView view
      import ViewHelpers.{ManageableObserver, toViewData}
      view addObserver this
      new Thread (() => {
        while(!_stop) {
          normalizeFrameRate(() => {
            if (_paused) this synchronized wait()
            view updateWorld (0, realTimeState getFilteredState(_ => true))
            //surgeon informAboutOrgansStatus view
            //println(stalker.report)
          }, frameRate)
        }
      }) start()
    }

    override def manage: ManageableController = this

    override def watch(entity: String): Unit = {
      //stalker.stalk(entity)
      surgeon inspects entity
    }

    override def unwatch(): Unit = surgeon leaves()

    override def add(entities: Map[String, Int]): Unit = {
      simulation.addEntities(entities)
    }

    override def entityData(id: String): Option[EntityState] = realTimeState getFilteredState(x => x.entityId == id) match {
      case Seq(single) => Some(single)
      case _ => None
    }

    override def play(): Unit = this synchronized {
      simulation play()
      _paused = false
      notify()
    }

    override def pause(): Unit = this synchronized {
      simulation pause()
      _paused = true
    }

    override def exit(): Unit = this synchronized {
      simulation dispose()
      _stop = true
      _paused = true
      notify()
    }

    private def normalizeFrameRate(job: () => Unit, fps: Int): Unit = {
      val start = System.currentTimeMillis()
      job()
      val stop = System.currentTimeMillis()
      if (stop - start < 1000 / fps) {
        Thread.sleep((1000 / fps) - (stop - start))
        //this synchronized wait((1000 / fps) - (stop - start))
      }
    }
  }
}
