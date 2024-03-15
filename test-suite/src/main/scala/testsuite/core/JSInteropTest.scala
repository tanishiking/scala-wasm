package testsuite.core

import scala.scalajs.js
import testsuite.Assert.ok

object JSInteropTest {
  def main(): Unit = {
    ok(
      testBasicTopLevel() &&
        testBasicStatic() &&
        testBasicInstance() &&
        testArray() &&
        testObject() &&
        testOperators() &&
        testLinkingInfo()
    )
  }

  def testBasicTopLevel(): Boolean = {
    ("" + js.undefined) == "undefined" &&
    js.eval("3 + 4").asInstanceOf[Int] == 7 &&
    js.isUndefined(())
  }

  def testBasicStatic(): Boolean = {
    js.Math.PI == 3.1415926535897932 &&
    js.Math.abs(-5.6) == 5.6 &&
    js.Math.clz32(6548) == 19 &&
    js.Math.min(5, 8, 2, 12, 3) == 2
  }

  def testBasicInstance(): Boolean = {
    val d = new js.Date(1710190169564.0)
    d.getTime() == 1710190169564.0 &&
    d.getUTCFullYear() == 2024 && {
      d.setTime(0.0)
      d.getTime() == 0.0
    }
  }

  def testArray(): Boolean = {
    val a = js.Array(1, 5, 3)
    a.length == 3 &&
    a(0) == 1 &&
    a(2) == 3 && {
      a(0) = 65
      a.push(78)
      a.length == 4 &&
      a(0) == 65 &&
      a(3) == 78
    }
  }

  def testObject(): Boolean = {
    val o = new js.Object().asInstanceOf[js.Dynamic]
    js.isUndefined(o.foo) && {
      o.foo = 5
      o.hasOwnProperty("foo").asInstanceOf[Boolean] &&
      o.foo.asInstanceOf[Int] == 5 && {
        js.special.delete(o, "foo")
        !o.hasOwnProperty("foo").asInstanceOf[Boolean]
      }
    }
  }

  def testOperators(): Boolean = {
    val x = 5.asInstanceOf[js.Dynamic]
    val y = 11.asInstanceOf[js.Dynamic]
    same(+x, 5) &&
    same(-x, -5) &&
    same(~x, -6) &&
    same(!x, false) &&
    same(js.typeOf(x), "number") && // regular typeof
    same(js.typeOf(js.Dynamic.global.Date), "function") && // typeof global ref
    same(x + y, 16) &&
    same(x - y, -6) &&
    same(x * y, 55) &&
    same(x / y, 0.45454545454545453) &&
    same(y % x, 1) &&
    same(x << 3.asInstanceOf[js.Dynamic], 40) &&
    same(x >> 1.asInstanceOf[js.Dynamic], 2) &&
    same(x >>> 2.asInstanceOf[js.Dynamic], 1) &&
    same(x & y, 1) &&
    same(x | y, 15) &&
    same(x ^ y, 14) &&
    same(x < y, true) &&
    same(x <= y, true) &&
    same(x > y, false) &&
    same(x >= y, false) &&
    same(x && y, 11) &&
    same(x || y, 5) &&
    same(x ** 3.asInstanceOf[js.Dynamic], 125)
  }

  def testLinkingInfo(): Boolean = {
    val linkingInfo = scala.scalajs.runtime.linkingInfo
    linkingInfo.esVersion >= 6 &&
    linkingInfo.assumingES6 == true
  }

  def same(a: js.Any, b: js.Any): Boolean = js.special.strictEquals(a, b)
}
