package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.view.configuration.Result

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

case class LoginDialog(window: Window) extends Dialog[Result] {
  initOwner(window)
  title = "Login Dialog"
  headerText = "Look, a Custom Login Dialog"

  // Set the button types.
  val loginButtonType = new ButtonType("Login", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(loginButtonType)

  // Create the username and password labels and fields.
  val username = new TextField() {
    promptText = "Username"
  }
  val password = new PasswordField() {
    promptText = "Password"
  }

  val grid = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Username:"), 0, 0)
    add(username, 1, 0)
    add(new Label("Password:"), 0, 1)
    add(password, 1, 1)
  }

  // Enable/Disable login button depending on whether a username was
  // entered.
  val loginButton = dialogPane().lookupButton(loginButtonType)
  loginButton.disable = true


  // Do some validation (disable when username is empty).
  username.text.onChange { (_, _, newValue) =>
    loginButton.disable = newValue.trim().isEmpty
  }

  dialogPane().content = grid

  // Request focus on the username field by default.
  Platform.runLater(username.requestFocus())

  // When the login button is clicked, convert the result to
  // a username-password-pair.
  resultConverter = dialogButton =>
    if (dialogButton == loginButtonType)
      Result(username.text(), password.text())
    else
      null


  def showAndThenPrint() = {
    this.showAndWait() match {
      case Some(Result(u, p)) => {
        println("Username=" + u + ", Password=" + p)
      }
      case None => println("Dialog returned: None")
    }
  }

}
