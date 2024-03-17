package testsuite.core

import scala.scalajs.js
import testsuite.Assert.ok

/** Test that we can manipulate instances of exception classes, but not throw them. */
object ThrowablesTest {
  def main(): Unit = {
    testBasicThrowable()
    testBasicException()

    testEmptyStackTrace()
  }

  def testBasicThrowable(): Unit = {
    val t = new Throwable("boom")
    ok(t.getMessage() == "boom")
    ok(t.getCause() == null)
    ok(t.toString() == "java.lang.Throwable: boom")

    val t2 = new Throwable("derived", t)
    ok(t2.getMessage() == "derived")
    ok(t2.getCause() eq t)
    ok(t2.toString() == "java.lang.Throwable: derived")
  }

  def testBasicException(): Unit = {
    val t = new Exception("boom")
    ok(t.getMessage() == "boom")
    ok(t.getCause() == null)
    ok(t.toString() == "java.lang.Exception: boom")

    val t2 = new Exception("derived", t)
    ok(t2.getMessage() == "derived")
    ok(t2.getCause() eq t)
    ok(t2.toString() == "java.lang.Exception: derived")
  }

  def testEmptyStackTrace(): Unit = {
    val t = new Throwable("boom")
    val stack = t.getStackTrace()
    ok(stack.length == 0)
  }
}
