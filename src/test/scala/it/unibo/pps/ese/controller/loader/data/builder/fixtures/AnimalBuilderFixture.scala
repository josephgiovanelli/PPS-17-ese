package it.unibo.pps.ese.controller.loader.data.builder.fixtures

trait AnimalBuilderFixture extends CustomGeneBuildFixture with DefaultGeneBuildFixture{
  def animalBFixture = new {
    val completeCusChr = Seq(customGBFixture.complete)
    val incompleteCusChr = Seq(customGBFixture.complete, customGBFixture.staticIncomplete)
    val completeDefChr = Seq(defaultGBFixture.complete)
    val incompleteDefChr = Seq(defaultGBFixture.complete, defaultGBFixture.dynamicIncomplete)
  }
}
