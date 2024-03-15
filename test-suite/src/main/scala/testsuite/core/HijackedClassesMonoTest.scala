package testsuite.core

import testsuite.Assert.ok

object HijackedClassesMonoTest {
  def main(): Unit = {
    ok(
      testInteger(5) &&
        testString("foo")
    )
  }

  def testInteger(x: Int): Boolean = {
    x.hashCode() == 5
  }

  def testString(foo: String): Boolean = {
    foo.length() == 3 &&
    foo.hashCode() == 101574
  }
}
