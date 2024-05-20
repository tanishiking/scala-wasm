package sample

import scala.scalajs.js
import scala.scalajs.js.annotation._
import _root_.scala.scalajs.wasm.io

object Main {
  @JSExportTopLevel("field")
  var exportedField: Int = 42

  @JSExportTopLevel("test")
  def test(i: Int): Boolean = {
    true
  }

  def main(args: Array[String]): Unit = {
    scala.scalajs.wasm.memory.withAllocator { allocator =>
      val segment = allocator.allocate(4)
      segment.setInt(0, 100)
      val result = segment.getInt(0)
      println(result)
    }
    io.printImpl("Hello from WASI!", newLine = true)
  }

  // Tested in SampleTest.scala
  def square(x: Int): Int = x * x

  private def println(x: Any): Unit =
    js.Dynamic.global.console.log("" + x)
}
