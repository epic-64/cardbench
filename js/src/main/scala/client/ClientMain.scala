package client

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import shared.Greeting

@main def main(): Unit =
  val container = dom.document.getElementById("app")
  container.innerHTML = "" // clear the loading spinner
  render(container, App.view())

object App:
  def view(): Element =
    div(
      cls := "app",
      h1(Greeting.message),
      p("Laminar + Cask + ScalaJS scaffold is running."),
    )
