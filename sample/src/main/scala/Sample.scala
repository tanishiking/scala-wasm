package sample

// import scala.scalajs.js.annotation._

object Main extends Base {
    // @JSExportTopLevel("foo")
    def foo(x: Int) = {
        ident(x)
        // val d = new Derived(1)
        // d.derived(x)
    }
}

class Base {
    def ident(x: Int) = x
}

object Foo {
    def foo =
        Main.ident(1)
}
// 
// class Derived(override val i: Int) extends Base(i) {
//     def derived(x: Int) = x * i
//     override def base(x: Int): Int = x * i
// }