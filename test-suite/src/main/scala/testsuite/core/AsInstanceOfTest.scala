package testsuite.core.asinstanceof

import scala.scalajs.js.annotation._

object AsInstanceOfTest {
  def main(): Unit = { val _ = test() }

  @JSExportTopLevel("asInstanceOf")
  def test(): Boolean = {
    testInt(5) &&
      testClasses(new Child())
  }

  def testClasses(c: Child): Boolean = {
    val c1 = c.asInstanceOf[Child]
    val c2 = c.asInstanceOf[Parent]
    c1.foo() == 5 && c2.foo() == 5
  }

  def testInt(x: Int): Boolean = {
    val x1 = x.asInstanceOf[Int]
    x1 == 5
  }

  class Parent {
    def foo(): Int = 5
  }
  class Child extends Parent
}
