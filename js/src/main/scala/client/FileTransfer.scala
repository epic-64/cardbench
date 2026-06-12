package client

import org.scalajs.dom
import scala.scalajs.js

/** Move text in and out of the browser as files: a download for export, a read
  * for import. The pickers and the object-URL lifecycle live here so the views
  * stay declarative.
  */
object FileTransfer:

  /** Hand `content` to the browser as a download named `filename`. Builds a
    * Blob, points a transient anchor at its object URL, clicks it, then revokes
    * the URL so it isn't leaked.
    */
  def download(filename: String, content: String): Unit =
    val opts = js.Dynamic.literal(`type` = "application/json").asInstanceOf[dom.BlobPropertyBag]
    val blob = new dom.Blob(js.Array[dom.BlobPart](content), opts)
    val url  = dom.URL.createObjectURL(blob)
    val a    = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
    a.href = url
    a.setAttribute("download", filename)
    dom.document.body.appendChild(a)
    a.click()
    dom.document.body.removeChild(a)
    dom.URL.revokeObjectURL(url)

  /** Read a chosen file's text, delivering it to `onText`. */
  def readText(file: dom.File, onText: String => Unit): Unit =
    val reader = new dom.FileReader()
    reader.onload = _ => onText(reader.result.asInstanceOf[String])
    reader.readAsText(file)
