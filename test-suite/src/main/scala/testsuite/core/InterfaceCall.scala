package testsuite.core

import testsuite.Assert.ok

object InterfaceCall {
  def main(): Unit = {
    val c = new Concrete()
    ok(c.plus(c.zero, 1) == 1 && c.minus(1, c.zero) == 1)
  }

  class Concrete extends AddSub with Zero {
    override def zero: Int = 0
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
