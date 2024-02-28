package sample

import scala.scalajs.js.annotation._

//
// class Base {
//   def sqrt(x: Int) = x * x
// }
//
object Main extends Eq {
  @JSExportTopLevel("test")
  def test(i: Int) = {
    val l = new Loop
    val r = new Rec
    eq(
        fib(l, i),
        fib(r, i)
    )

  }
  def fib(fib: Fib, n: Int): Int = fib.fib(n)
}

class Loop extends LoopFib
class Rec extends RecFib

trait Eq {
    def eq(a: Int, b: Int): Boolean = a == b
}

trait LoopFib extends Fib {
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

trait RecFib extends Fib {
  def fib(n: Int): Int =
    if (n <= 1) {
      n
    } else {
      fib(n - 1) + fib(n - 2)
    }
}

trait Fib {
  def fib(n: Int): Int
  //  = {
  //   if (n <= 1) {
  //     n
  //   } else {
  //     fib(n - 1) + fib(n - 2)
  //   }
  // }

}

//
//
// object Bar {
//   def bar(b: Base) = b.base
// }

// class Base extends Incr {
//   override def incr(x: Int) = foo(x) + 1
// }
//
// trait Incr extends BaseTrait {
//     // val one = 1
//     def incr(x: Int): Int
// }
//
// trait BaseTrait {
//     def foo(x: Int) = x
// }

// object Foo {
//     def foo =
//         Main.ident(1)
// }
//
// class Derived(override val i: Int) extends Base(i) {
//     def derived(x: Int) = x * i
//     override def base(x: Int): Int = x * i
// }
