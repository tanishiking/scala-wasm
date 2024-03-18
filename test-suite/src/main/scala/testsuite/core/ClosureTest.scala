package testsuite.core

import scala.scalajs.js

import testsuite.Assert.ok

object ClosureTest {
  def main(): Unit = {
    testClosure()
    testClosureThis()

    // TODO We cannot test closures with ...rest params yet because they need Seq's

    testGiveToActualJSCode()
  }

  def testClosure(): Unit = {
    def makeClosure(x: Int, y: String): js.Function2[Boolean, String, String] =
      (z, w) => s"$x $y $z $w"

    val f = makeClosure(5, "foo")
    ok(f(true, "bar") == "5 foo true bar")
  }

  def testClosureThis(): Unit = {
    def makeClosure(x: Int, y: String): js.ThisFunction2[Any, Boolean, String, String] =
      (ths, z, w) => s"$ths $x $y $z $w"

    val f = makeClosure(5, "foo")
    ok(f(new Obj, true, "bar") == "Obj 5 foo true bar")
  }

  def testGiveToActualJSCode(): Unit = {
    val arr = js.Array(2, 3, 5, 7, 11)
    val f: js.Function1[Int, Int] = x => x * 2
    val result = arr.asInstanceOf[js.Dynamic].map(f).asInstanceOf[js.Array[Int]]
    ok(result.length == 5)
    ok(result(0) == 4)
    ok(result(1) == 6)
    ok(result(2) == 10)
    ok(result(3) == 14)
    ok(result(4) == 22)
  }

  class Obj {
    override def toString(): String = "Obj"
  }
}
