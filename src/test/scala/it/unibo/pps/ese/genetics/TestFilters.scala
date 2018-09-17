package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genericworld.model.EntityInfo
import it.unibo.pps.ese.view.filters.{EntityFiltersValues, PlantFiltersValues}
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
