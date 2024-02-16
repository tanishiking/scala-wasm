package sample

import scala.scalajs.js.annotation._

object Main extends Base {
  def multiply(x: Int, y: Int) = x * y

  @JSExportTopLevel("multSqrt")
  def multSqrt(x: Int, y: Int) =
    multiply(sqrt(x), sqrt(y))
}

class Base {
  def sqrt(x: Int) = x * x
}

// object Foo {
//     def foo =
//         Main.ident(1)
// }
//
// class Derived(override val i: Int) extends Base(i) {
//     def derived(x: Int) = x * i
//     override def base(x: Int): Int = x * i
// }
