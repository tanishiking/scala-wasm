package testsuite.core.virtualdispatch

import scala.scalajs.js.annotation._

object VirtualDispatch {
  def main(): Unit = { val _ = test() }

  @JSExportTopLevel("virtualDispatch")
  def test(): Boolean = {
    val a = new A
    val b = new B

    testA(a) &&
    testB(a, isInstanceOfA = true) &&
    testB(b, isInstanceOfA = false) &&
    testC(a, isInstanceOfA = true) &&
    testC(b, isInstanceOfA = false)
  }

  def testA(a: A): Boolean = {
    a.a == 2 && a.impl == 2 && a.b == 1 && a.c == 1
  }

  def testB(b: B, isInstanceOfA: Boolean): Boolean = {
    if (isInstanceOfA) {
      b.b == 1 && b.c == 1 && b.impl == 2
    } else {
      b.b == 1 && b.c == 1 && b.impl == 0
    }
  }

  def testC(c: C, isInstanceOfA: Boolean): Boolean = {
    if (isInstanceOfA) {
      c.c == 1 && c.impl == 2
    } else {
      c.c == 1 && c.impl == 0
    }
  }

  class A extends B {
    def a: Int = 2
    override def impl = 2
  }

  class B extends C {
    def b: Int = 1
    override def c: Int = 1
  }

  abstract class C {
    def c: Int
    def impl: Int = 0
  }
}
