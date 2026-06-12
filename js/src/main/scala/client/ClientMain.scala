package client

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@main def main(): Unit =
  val container = dom.document.getElementById("app")
  container.innerHTML = ""             // clear the loading spinner
  container.setAttribute("class", "app-root") // swap the loader's centering for a full-height app frame
  render(container, AppView.view())
