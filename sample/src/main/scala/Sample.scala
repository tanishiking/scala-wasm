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

  def main(args: Array[String]): Unit = {
    println("hello world")
  }

  // Tested in SampleTest.scala
  def square(x: Int): Int = x * x

  private def println(x: Any): Unit =
    js.Dynamic.global.console.log("" + x)
}
