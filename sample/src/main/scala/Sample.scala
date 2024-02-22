package sample

// import scala.scalajs.js.annotation._

// object Main extends Base {
//   def multiply(x: Int, y: Int) = x * y
// 
//   @JSExportTopLevel("multSqrt")
//   def multSqrt(x: Int, y: Int) =
//     multiply(sqrt(x), sqrt(y))
// }
// 
// class Base {
//   def sqrt(x: Int) = x * x
// }
//
class Derived extends IFace

trait IFace {
    def iface: Int = 1
}

object Foo {
    def foo(x: IFace) = x.iface
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
