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
    val d = new Derived
    foo(d)
  }
  def foo(x: IFace) = x.iface
}

class Derived extends IFace {
    val y = 1
    override def iface: Int = {
        incr
        incr
        x + y
    }
}

trait IFace {
    var x = 1
    def iface: Int
    def incr = x += 1
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
