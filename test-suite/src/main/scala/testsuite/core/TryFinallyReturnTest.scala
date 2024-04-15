package testsuite.core

import testsuite.Assert.assertSame

/** Imported from the Scala test case `test/files/run/finally.scala`. */
object TryFinallyReturnTest {
  // Simulate a `println` API that we can check afterwards
  var printlnOutput: String = ""

  def println(x: Any): Unit =
    printlnOutput = printlnOutput + x + "\n"

  val expectedOutput = raw"""Running throwCatchFinally
hi
In Finally
java.lang.RuntimeException: ouch
----------------------------------------
Running retCatch
java.lang.Exception
in finally
----------------------------------------
Running throwCatch
java.lang.Exception
in finally
CAUGHT: java.lang.Exception
----------------------------------------
Running retBody
in finally
----------------------------------------
Running throwBody
java.lang.Exception
in finally
----------------------------------------
Running retFinally
body
in finally 1
in finally 2
----------------------------------------
Running throwFinally
body
in finally
java.lang.Exception
----------------------------------------
Running nestedFinallyBlocks
in finally 1
in finally 2
----------------------------------------
Running nestedFinallyBlocks2
in try 1
in finally 1
in fall-through
in finally 2
----------------------------------------
"""

  def main(): Unit = {
    test(throwCatchFinally(), "throwCatchFinally")
    test(retCatch(), "retCatch")
    test(throwCatch(), "throwCatch")
    test(retBody(), "retBody")
    test(throwBody(), "throwBody")
    test(retFinally(), "retFinally")
    test(throwFinally(), "throwFinally")
    test(nestedFinallyBlocks(), "nestedFinallyBlocks")
    test(nestedFinallyBlocks2(), "nestedFinallyBlocks2")

    assertSame(expectedOutput, printlnOutput)
  }

  // test that finally is not covered by any exception handlers.
  def throwCatchFinally(): Unit = {
    try {
      bar()
    } catch {
      case e: Throwable => println(e)
    }
  }

  // test that finally is not covered by any exception handlers.
  def bar(): Unit = {
    try {
      println("hi")
    } catch {
      case e: Throwable => println("SHOULD NOT GET HERE")
    } finally {
      println("In Finally")
      throw new RuntimeException("ouch")
    }
  }

  // return in catch (finally is executed)
  def retCatch(): Unit = {
    try {
      throw new Exception
    } catch {
      case e: Throwable =>
        println(e);
        return
    } finally println("in finally")
  }

  // throw in catch (finally is executed, exception propagated)
  def throwCatch(): Unit = {
    try {
      throw new Exception
    } catch {
      case e: Throwable =>
        println(e);
        throw e
    } finally println("in finally")
  }

  // return inside body (finally is executed)
  def retBody(): Unit = {
    try {
      return
    } catch {
      case e: Throwable =>
        println(e);
        throw e
    } finally println("in finally")
  }

  // throw inside body (finally and catch are executed)
  def throwBody(): Unit = {
    try {
      throw new Exception
    } catch {
      case e: Throwable =>
        println(e);
    } finally println("in finally")
  }

  // return inside finally (each finally is executed once)
  def retFinally(): Unit = {
    try {
      try println("body")
      finally {
        println("in finally 1")
        return
      }
    } finally println("in finally 2")
  }

  // throw inside finally (finally is executed once, exception is propagated)
  def throwFinally(): Unit = {
    try {
      try println("body")
      finally {
        println("in finally")
        throw new Exception
      }
    } catch {
      case e: Throwable => println(e)
    }
  }

  // nested finally blocks with return value
  def nestedFinallyBlocks(): Int =
    try {
      try {
        return 10
      } finally {
        try { () }
        catch { case _: Throwable => () }
        println("in finally 1")
      }
    } finally {
      println("in finally 2")
    }

  def nestedFinallyBlocks2(): Int = {
    try {
      try {
        println("in try 1")
      } finally {
        println("in finally 1")
      }
      println("in fall-through")
      0
    } finally {
      println("in finally 2")
    }
  }

  def test[A](m: => A, name: String): Unit = {
    println("Running %s".format(name))
    try {
      m
    } catch {
      case e: Throwable => println("CAUGHT: " + e)
    }
    println("-" * 40)
  }
}
