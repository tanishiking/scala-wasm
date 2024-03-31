package testsuite.core

import testsuite.Assert.ok

object InterfaceCall {
  def main(): Unit = {
    val c = new Concrete()
    val cAddSub: AddSub = c
    val cZero: Zero = c
    ok(cAddSub.plus(5, 7) == 12)
    ok(cAddSub.minus(5, 7) == -2)
    ok(cZero.zero == 0)
    ok(cAddSub.plus(cZero.zero, 1) == 1 && cAddSub.minus(1, cZero.zero) == 1)

    val o = new OtherConcrete()
    val oAddSub: AddSub = o
    val oZero: Zero = o
    ok(oAddSub.plus(5, 7) == 12)
    ok(oAddSub.minus(5, 7) == -2)
    ok(oZero.zero == 5)
    ok(oAddSub.plus(oZero.zero, 1) == 6 && oAddSub.minus(1, oZero.zero) == -4)
  }

  class Concrete extends AddSub with Zero {
    override def zero: Int = 0
  }

  class OtherConcrete extends Zero with Sub with AddSub {
    override def zero: Int = 5
  }

  trait Adder {
    def plus(a: Int, b: Int) = a + b
  }

  trait Sub {
    def minus(a: Int, b: Int): Int = a - b
  }

  trait AddSub extends Adder with Sub

  trait Zero {
    def zero: Int
  }
}
