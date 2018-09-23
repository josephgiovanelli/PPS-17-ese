package it.unibo.pps.ese.model.genetics

import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityInfo
import it.unibo.pps.ese.view.sections.filters.{EntityFiltersValues, PlantFiltersValues}
import org.scalatest.FunSuite
import it.unibo.pps.ese.view.utilities.EntityConversions._

class TestFilters extends FunSuite{
  test("Test the filters"){
    val filter:EntityFiltersValues = PlantFiltersValues(None,None,Map())
    case class TestEntity() extends EntityInfo{
    }
    val entity = TestEntity()
    println(entity.applyFilter(filter))
  }
}
