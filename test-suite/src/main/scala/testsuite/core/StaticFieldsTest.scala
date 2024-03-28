package testsuite.core

import scala.scalajs.js.annotation.JSExportTopLevel

import testsuite.Assert.assertSame

object StaticFieldsTest {
  /* We use @JSExportTopLevel to force the fields to be encoded as static in,
   * the IR; not for the exported behavior per se.
   */

  @JSExportTopLevel("StaticFieldsTest_valStatic")
  val valStatic: Int = 42

  @JSExportTopLevel("StaticFieldsTest_varStatic")
  var varStatic: String = "hello"

  def main(): Unit = {
    assertSame(42, valStatic)
    assertSame("hello", varStatic)
    varStatic = "foo"
    assertSame("foo", varStatic)
  }
}
