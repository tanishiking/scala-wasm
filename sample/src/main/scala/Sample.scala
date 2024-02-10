package sample

import scala.scalajs.js.annotation._

object Main {
    // private val field = 1

    @JSExportTopLevel("foo")
    def foo(x: Int) = x + 1
}