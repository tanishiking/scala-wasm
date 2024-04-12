package testsuite.core

import scala.scalajs.js
import scala.scalajs.js.annotation._

object JSExportTopLevelTest {
  def main(): Unit = {
    /* The main() function of this test does nothing.
     * The real test happens after loading the module from TestSuites.scala.
     */
    ()
  }

  @JSExportTopLevel("immutableField")
  val immutableField: String = "my immutable field value"

  @JSExportTopLevel("mutableField")
  var mutableField: Int = 42

  @JSExportTopLevel("simpleFunction")
  def simpleFunction(x: Int): Int =
    x * x

  @JSExportTopLevel("functionWithRest")
  def functionWithRest(x: Int, rest: Int*): Int =
    x * rest.sum

  @JSExportTopLevel("SimpleClass")
  class SimpleClass(val foo: Int) extends js.Object

  @JSExportTopLevel("SimpleObject")
  object SimpleObject extends js.Object {
    val bar: String = "the bar field"
  }
}
