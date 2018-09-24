package it.unibo.pps.ese.view.sections.genome

import scalafx.scene.paint.{Color, PhongMaterial}

object Materials {
  val blueMaterial:PhongMaterial = new PhongMaterial {
    diffuseColor = Color.web("1abc9c")
    specularColor = Color.web("16a085")
  }
  val greyMaterial:PhongMaterial = new PhongMaterial {
    diffuseColor = Color.web("95a5a6")
    specularColor = Color.web("7f8c8d")
  }
  val whiteMaterial:PhongMaterial = new PhongMaterial{
    diffuseColor = Color.web("ecf0f1")
    specularColor = Color.web("bdc3c7")
  }
}
