package testsuite.core

import scala.scalajs.js

import testsuite.Assert.{assertSame, fail}

object WrapUnwrapThrowableTest {
  def main(): Unit = {
    testWrapAsThrowable()
    testUnwrapFromThrowable()
  }

  def testWrapAsThrowable(): Unit = {
    // Wraps a js.Object
    val obj = new js.Object
    val e1 = js.special.wrapAsThrowable(obj)
    e1 match {
      case e1: js.JavaScriptException => assertSame(obj, e1.exception)
      case _                          => fail()
    }

    // Wraps null
    val e2 = js.special.wrapAsThrowable(null)
    e2 match {
      case e2: js.JavaScriptException => assertSame(null, e2.exception)
      case _                          => fail()
    }

    // Does not wrap a Throwable
    val th = new IllegalArgumentException
    assertSame(th, js.special.wrapAsThrowable(th))

    // Does not double-wrap
    assertSame(e1, js.special.wrapAsThrowable(e1))
  }

  def testUnwrapFromThrowable(): Unit = {
    // Unwraps a JavaScriptException
    val obj = new js.Object
    assertSame(obj, js.special.unwrapFromThrowable(new js.JavaScriptException(obj)))

    // Does not unwrap a Throwable
    val th = new IllegalArgumentException
    assertSame(th, js.special.unwrapFromThrowable(th))
  }
}
