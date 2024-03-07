package testsuite.core.add

import scala.scalajs.js.annotation._

object Add {
  def main(): Unit = { val _ = test() }
  @JSExportTopLevel("add")
  def test(): Boolean = {
    1 + 1 == 2
  }
}
