package it.unibo.pps.ese.view.speciesdetails

import scalafx.scene.image.Image
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
  val DIFFUSE_MAP:String =
//    "https://oli.org/images/OLI_DriveSafe/BRAIN%20BACKGROUND.png"
//    "https://previews.123rf.com/images/albund/albund1408/albund140800165/31244847-a-close-up-texture-of-regular-human-brain-tissue.jpg"
"it/unibo/pps/ese/controller/brain-patterns.png"
  val NORMAL_MAP:String =
//    "https://www.filterforge.com/filters/1910-bump.jpg"
    "it/unibo/pps/ese/controller/NormalMap.png"
  val SPECULAR_MAP:String =
    "https://www.filterforge.com/filters/1910-normal.jpg"
  val redMaterial = new PhongMaterial {
    //      diffuseColor = Color.Pink
//          specularColor = Color.Red
    specularMap = new Image(
            NORMAL_MAP,
            500,
            500,
            true,
            true
          )
    diffuseMap = new Image(
      DIFFUSE_MAP,
      500,
      500,
      true,
      true
    )
//    bumpMap = new Image(
//            NORMAL_MAP,
//            500,
//            500,
//            true,
//            true)
  }
}
