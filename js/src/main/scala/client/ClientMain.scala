package client

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@main def main(): Unit =
  val container = dom.document.getElementById("app")
  container.innerHTML = "" // clear the loading spinner
  render(container, GameView.view())
