package testsuite.core.hijackedclassesmono

import scala.scalajs.js.annotation._

object HijackedClassesMonoTest {
  def main(): Unit = { val _ = test() }

  @JSExportTopLevel("hijackedClassesMono")
  def test(): Boolean = {
    testInteger(5) &&
      testString("foo")
  }

  def testInteger(x: Int): Boolean = {
    x.hashCode() == 5
  }

  def testString(foo: String): Boolean = {
    foo.length() == 3 &&
      foo.hashCode() == 101574
  }
}
