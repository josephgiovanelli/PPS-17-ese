package it.unibo.pps.ese.genericworld

import java.util.UUID
import java.util.UUID.randomUUID

import it.unibo.pps.ese.Launcher.{parameters, stage}
import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl
import it.unibo.pps.ese.entitybehaviors._
import it.unibo.pps.ese.genericworld.controller.Controller
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genetics.dna.AnimalGenome
import it.unibo.pps.ese.genetics.entities.QualityType._
import it.unibo.pps.ese.genetics.entities.{Animal, AnimalInfo, DietType, Species}
import it.unibo.pps.ese.genetics.generators.SpeciesUtilities
import it.unibo.pps.ese.genetics.generators.data.{InputDataAdapter, TranslatedAnimalData}
import it.unibo.pps.ese.utils.Point
import it.unibo.pps.ese.view.View
import it.unibo.pps.ese.view.ViewLauncher.{stage, view}
import scalafx.application.JFXApp
import scalafx.stage.WindowEvent

import scala.concurrent.duration._
import scala.concurrent.Await
import scalafx.Includes._

import scala.concurrent.ExecutionContext.Implicits.global

object TestLauncher extends JFXApp {

//    val entity1 = Entity("improved", "1")
//    val component3 = BrainComponent(entity1 specifications, Point(7, 6), 6, 6, 6, "herbivore", 5, 20)
//    val component4 = PhysicalStatusComponent(entity1 specifications, 50, 4, 4, 10, 30, 0.3, 2)
//    entity1 addComponent component3
//    entity1 addComponent component4
//
//    val entity4 = Entity("improved", "4")
//    val component1 = BrainComponent(entity4 specifications, Point(3, 3), 10, 10, 10, "carnivorous", 5, 20)
//    val component2 = PhysicalStatusComponent(entity4 specifications, 50, 4, 10, 10, 30, 0.3, 2)
//    entity4 addComponent component1
//    entity4 addComponent component2
//
//    world addEntity entity4
//    world addEntity entity1

//    val controller: Controller =
//      SimulationBuilder buildWorldFromSimulationData ("it/unibo/pps/ese/controller/loader/Simulation.yml", 500, 500)

  YamlLoader.loadSimulation("it/unibo/pps/ese/controller/loader/Simulation.yml") match {
    case data: CompleteSimulationData =>
      val controller: Controller =
        new SimulationBuilder[EmptySimulation]
          .dimension(500, 500)
          .data(data)
          .build

      val view = View(GeneticsSimulator)
      stage = view
      controller attachView (view, frameRate = 30)
      controller.manage.play()

      stage.setOnCloseRequest((e: WindowEvent) => {
        controller.manage.exit()
      })
    case _ =>
      throw new IllegalArgumentException
  }

//    println("Playing")
//    Thread.sleep(1000)
//
//    controller.pause()
//    println("Paused")
//    Thread.sleep(1000)
//
//    controller.play()
//
//    println("Playing")
//    Thread.sleep(1000)
//
//    println("Stopped")
//    controller.exit()
}
