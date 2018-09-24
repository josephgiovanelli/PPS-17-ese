package it.unibo.pps.ese.view.sections.history

import de.jensd.fx.glyphs.emojione.{EmojiOne, EmojiOneView}
import de.jensd.fx.glyphs.materialdesignicons.{MaterialDesignIcon, MaterialDesignIconView}
import javafx.scene.text.Font
import scalafx.Includes._
import scalafx.scene.control.{Label, ListCell}
import scalafx.scene.layout.HBox
import scalaz._
import Scalaz._

/**
  * Custom [[ListCell]] to visualize the logs
  */
class LogListViewCell extends ListCell[Log]{

  prefHeight = 30
  item.onChange{ (_,_,log)=>
    graphic = (log != null)?{
      val hBox = new HBox(5)
      val label = new Label(log.logText)
      val icon = log.logType match {
        case BornLog => new MaterialDesignIconView(MaterialDesignIcon.BABY_BUGGY)
        case DeadLog =>new EmojiOneView(EmojiOne.REMINDER_RIBBON)
        case MutationLog => new MaterialDesignIconView(MaterialDesignIcon.BIOHAZARD)
        case CouplingLog => new MaterialDesignIconView(MaterialDesignIcon.HEART)
        case ExtinctionLog => new EmojiOneView(EmojiOne.SKULL_CROSSBONES)
        case PopulationLog => new MaterialDesignIconView(MaterialDesignIcon.CHART_AREASPLINE)
        case GenerationLog => new MaterialDesignIconView(MaterialDesignIcon.CALENDAR_CLOCK)
        case MostPopulousLog => new MaterialDesignIconView(MaterialDesignIcon.CROWN);
        case NewEraLog => new MaterialDesignIconView(MaterialDesignIcon.NEWSPAPER)
      }
      label.font = Font.font("Calibri", 16)
      icon.setGlyphSize(18)
      hBox.children.clear()
      hBox.children+= icon
      hBox.children += label
      hBox
    }| null
  }
}
