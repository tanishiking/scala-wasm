package testsuite.core

import testsuite.Assert._

object ThrowAndTryTest {
  def main(): Unit = {
    testTryCatch()
    testTryCatchWithRefResultType()
    testTryFinally()
  }

  private def testTryCatch(): Unit = {
    try {
      doNotThrow()
    } catch {
      case e: IllegalArgumentException =>
        fail()
    }

    try {
      throwIllegalArgument()
      fail()
    } catch {
      case e: IllegalArgumentException =>
        assertSame("boom", e.getMessage())
    }

    try {
      try {
        throwIllegalArgument()
        fail()
      } catch {
        case e: UnsupportedOperationException =>
          // This one should not be caught
          fail()
      }
      fail() // the exception should have been rethrown
    } catch {
      case e: IllegalArgumentException =>
        assertSame("boom", e.getMessage())
    }
  }

  private class A(val value: Int)

  private def testTryCatchWithRefResultType(): Unit = {
    val x =
      try {
        new A(doNotThrow())
      } catch {
        case e: IllegalArgumentException => new A(20)
      }
    assertSame(5, x.value)

    val y =
      try {
        new A(throwIllegalArgument())
      } catch {
        case e: IllegalArgumentException => new A(20)
      }
    assertSame(20, y.value)
  }

  private def testTryFinally(): Unit = {
    var didFinally = false

    try {
      try {
        throwIllegalArgument()
        fail()
      } finally {
        didFinally = true
      }
      fail() // the exception should have been rethrown
    } catch {
      case e: IllegalArgumentException => ()
    }
    ok(didFinally)

    didFinally = false
    try {
      doNotThrow()
    } finally {
      didFinally = true
    }
    ok(didFinally)
  }

  private def throwIllegalArgument(): Int =
    throw new IllegalArgumentException("boom")

  private def doNotThrow(): Int = 5
}
