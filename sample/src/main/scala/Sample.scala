package sample

import scala.scalajs.js
import scala.scalajs.js.annotation._

object Main {
  @JSExportTopLevel("test")
  def test(i: Int): Boolean = {
    js.Dynamic.global.console.log("Hello")
    true
  }
}
