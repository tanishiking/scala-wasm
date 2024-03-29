package sample

import scala.scalajs.js
import scala.scalajs.js.annotation._

object Main {
  @JSExportTopLevel("field")
  var exportedField: Int = 42

  @JSExportTopLevel("test")
  def test(i: Int): Boolean = {
    println("Hello")
    exportedField = 53
    println(exportedField)
    true
  }

  private def println(x: Any): Unit =
    js.Dynamic.global.console.log("" + x)
}
