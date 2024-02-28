package sample

import scala.scalajs.js.annotation._

// 
// class Base {
//   def sqrt(x: Int) = x * x
// }
//
object Main {
  @JSExportTopLevel("test")
  def test() = {
    val d = new Derived2
    incr(d, plus(d, 1, 2))
  }
  def plus(x: Base1, a: Int, b: Int) = x.plus(a, b)
  def incr(x: Derived, a: Int): Int = x.incr(a)
}

class Derived2 extends Derived {
    override def incr(x: Int) = super.incr(x)
}
class Derived extends Base1

trait Base1 extends Base2 {
    def incr(x: Int): Int = x + 1
}

trait Base2 {
    def plus(a: Int, b: Int) = a + b

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
