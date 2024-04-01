package testsuite.core

import scala.scalajs.js

import testsuite.Assert._

object JSForInTest {
  def main(): Unit = {
    val obj = new js.Object().asInstanceOf[js.Dynamic]
    obj.foo = "foobar"
    obj.bar = 5

    var iter = 0

    js.special.forin(obj) { key =>
      if (iter == 0)
        assertSame("foo", key)
      else
        assertSame("bar", key)
      iter += 1
    }

    assertSame(2, iter)
  }
}
