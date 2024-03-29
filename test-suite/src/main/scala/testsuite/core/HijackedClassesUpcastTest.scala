package testsuite.core

import testsuite.Assert.ok

object HijackedClassesUpcastTest {
  def main(): Unit = {
    ok(
      testBoolean(true) &&
        testInteger(5) &&
        testIntegerNull(null) &&
        testString("foo") &&
        testStringNull(null) &&
        testCharacter('A')
    )

  }

  def testBoolean(x: Boolean): Boolean = {
    val x1 = identity(x)
    x1 && {
      val x2: Any = x1
      x2 match {
        case x3: Boolean => x3
        case _           => false
      }
    }
  }

  def testInteger(x: Int): Boolean = {
    val x1 = identity(x)
    x1 == 5 && {
      val x2: Any = x1
      x2 match {
        case x3: Int => x3 + 1 == 6
        case _       => false
      }
    }
  }

  def testIntegerNull(x: Any): Boolean = {
    !x.isInstanceOf[Int] &&
    !x.isInstanceOf[java.lang.Integer] &&
    (x.asInstanceOf[Int] == 0) && {
      val x2 = x.asInstanceOf[java.lang.Integer]
      x2 == null
    }
  }

  def testString(x: String): Boolean = {
    val x1 = identity(x)
    x1.length() == 3 && {
      val x2: Any = x1
      x2 match {
        case x3: String => x3.length() == 3
        case _          => false
      }
    }
  }

  def testStringNull(x: Any): Boolean = {
    !x.isInstanceOf[String] && {
      val x2 = x.asInstanceOf[String]
      x2 == null
    }
  }

  def testCharacter(x: Char): Boolean = {
    val x1 = identity(x)
    x1 == 'A' && {
      val x2: Any = x1
      x2 match {
        case x3: Char => (x3 + 1).toChar == 'B'
        case _        => false
      }
    }
  }

  @noinline
  def identity[A](x: A): A = x
}
