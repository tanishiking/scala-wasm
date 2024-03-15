package testsuite.core

import testsuite.Assert.ok

object AsInstanceOfTest {
  def main(): Unit = {
    ok(
      testInt(5) &&
        testClasses(new Child()) &&
        testString("foo", true)
    )
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

  def testString(s: String, b: Boolean): Boolean = {
    val s1 = s.asInstanceOf[String]
    val s2 = ("" + b).asInstanceOf[String]
    s1.length() == 3 && s2.length() == 4
  }

  class Parent {
    def foo(): Int = 5
  }
  class Child extends Parent
}
