package testsuite.core.simple

import scala.scalajs.js.annotation._

object Simple {
  def main(): Unit = { val _ = test() }
  @JSExportTopLevel("simple")
  def test(): Boolean = {
    true
  }
}
