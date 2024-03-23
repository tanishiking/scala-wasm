package sample

import scala.annotation.tailrec

import scala.scalajs.js
import scala.scalajs.js.annotation._

object Main {
  @JSExportTopLevel("test")
  def test(i: Int): Boolean = {
    val foo = new Foo(i)
    val clone = foo.clone().asInstanceOf[Foo]
    clone.x == i
  }
}

class Foo(val x: Int) extends Cloneable {
  override def clone(): Object = super.clone()
}
