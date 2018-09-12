package it.unibo.pps.ese.view
import de.jensd.fx.glyphs.emojione.EmojiOneView
import de.jensd.fx.glyphs.emojione.EmojiOne
import de.jensd.fx.glyphs.materialdesignicons.{MaterialDesignIcon, MaterialDesignIconView}
import javafx.scene.text.Font
import scalafx.scene.control.{Label, ListCell, ListView}
import scalafx.scene.layout.HBox
import scalafx.Includes._


class LogListViewCell extends ListCell[Log]{
  val hBox = new HBox(5)
  graphic = hBox
  prefHeight = 30
  item.onChange{ (_,_,log)=>
    if(log != null){
      val label = new Label(log.logText)
      val icon = log.logType match {
        case BornLog => new MaterialDesignIconView(MaterialDesignIcon.BABY_BUGGY)
        case DeadLog =>new EmojiOneView(EmojiOne.REMINDER_RIBBON)
        case MutationLog => new MaterialDesignIconView(MaterialDesignIcon.BIOHAZARD)
        case CouplingLog => new MaterialDesignIconView(MaterialDesignIcon.HEART)
        case ExtinctionLog => new EmojiOneView(EmojiOne.SKULL_CROSSBONES)
      }
      label.font = Font.font("Calibri", 16)
      icon.setGlyphSize(18)
      hBox.children.clear()
      hBox.children+= icon
      hBox.children += label
    }
  }
}
