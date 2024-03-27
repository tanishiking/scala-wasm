package testsuite.core

import scala.annotation.tailrec
import testsuite.Assert.ok

object ControlStructuresTest {
  def main(): Unit = {
    testIf()
    testWhile()
    testFib()
  }

  private def testIf(): Unit = {
    def test(x: Int): String =
      if (x < 0) "negative"
      else if (x == 0) "zero"
      else "positive"
    ok(test(1) == "positive")
    ok(test(0) == "zero")
    ok(test(-1) == "negative")
  }

  private def testWhile(): Unit = {
    var x = 10
    while (x > 0) { x = x - 1 }
    ok(x == 0)
  }

  private def testFib(): Unit = {
    def testFib(i: Int, expected: Int): Unit = {
      ok(RecFib.fib(i) == expected)
      ok(LoopFib.fib(i) == expected)
      ok(TailRecFib.fib(i) == expected)
    }
    testFib(0, 0)
    testFib(1, 1)
    testFib(2, 1)
    testFib(3, 2)
    testFib(4, 3)
    testFib(5, 5)
    testFib(6, 8)
  }
}

object RecFib {
  def fib(n: Int): Int = if (n <= 1) n else fib(n - 1) + fib(n - 2)
}

object LoopFib {
  def fib(n: Int): Int = {
    var a = 0
    var b = 1
    var i = 0
    while (i < n) {
      val temp = b
      b = a + b
      a = temp
      i += 1
    }
    a
  }
}

object TailRecFib {
  def fib(n: Int): Int = fibLoop(n, 0, 1)
  @tailrec
  private final def fibLoop(n: Int, a: Int, b: Int): Int =
    if (n == 0) a
    else fibLoop(n - 1, b, a + b)
}
