package testsuite.core

import testsuite.Assert.ok

object FieldsTest {
  def main(): Unit = {
    val parent = new Parent(5)
    ok(parent.x == 5)
    ok(parent.getX == 5)

    val child = new Child(6, "foo")
    ok(child.x == 6)
    ok(child.getX == 6)
    ok(child.foo() == 3)

    val child2 = new Child(-6, "foo")
    ok(child2.x == -6)
    ok(child2.getX == -6)
    ok(child2.foo() == -3)
  }

  class Parent(val x: Int) {
    def getX: Int = x
  }

  class Child(x2: Int, y: String) extends Parent(x2) {
    def foo(): Int = if (x >= 0) y.length() else -y.length()
  }
}
