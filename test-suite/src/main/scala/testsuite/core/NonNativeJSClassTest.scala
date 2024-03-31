package testsuite.core

import scala.scalajs.js

import testsuite.Assert._

object NonNativeJSClassTest {
  def main(): Unit = {
    testBasic()
    testClassCaptures()
    testInheritanceAndSuper()
    testObject()
  }

  def testBasic(): Unit = {
    val o = new MyClass("string param")

    // Field access and initialization
    assertSame(0, o.asInstanceOf[js.Dynamic].i)
    assertSame(0, o.i)
    o.i = 42
    assertSame(42, o.i)

    // Method call
    assertSame(8, o.foobar(5))

    // Property read/write
    assertSame("prop string param", o.prop)
    o.prop = "foobar"
    assertSame("prop set foobar", o.prop)

    // Instance test -- also ensures that we get the same MyClass class
    assertSame(true, (o: Any).isInstanceOf[MyClass])
  }

  def testClassCaptures(): Unit = {
    def localJSClass(capture: Int): js.Dynamic = {
      class Local(s: String) extends MyClass(s) {
        def getCapture(): Int = capture
      }
      js.constructorOf[Local]
    }

    // Create classes

    val localClass1 = localJSClass(5)
    assertSame("function", js.typeOf(localClass1))
    val localClass2 = localJSClass(6)
    assertSame(true, localClass2 ne localClass1)

    val obj1 = js.Dynamic.newInstance(localClass1)("foobar")
    assertSame("prop foobar", obj1.prop)
    assertSame(5, obj1.getCapture())

    val obj2 = js.Dynamic.newInstance(localClass2)("babar")
    assertSame("prop babar", obj2.prop)
    assertSame(6, obj2.getCapture())

    // Instance tests -- the two local classes are independent
    assertSame(true, (obj1: Any).isInstanceOf[MyClass])
    assertSame(true, js.special.instanceof(obj1, localClass1))
    assertSame(false, js.special.instanceof(obj1, localClass2))
    assertSame(true, js.special.instanceof(obj2, localClass2))
  }

  def testInheritanceAndSuper(): Unit = {
    val sub = new Sub()

    assertSame("prop subarg sub", sub.prop)
    sub.prop = "new value"
    assertSame("prop set from sub new value sub", sub.prop)

    assertSame(15, sub.foobar(6))

    assertSame(true, (sub: Any).isInstanceOf[MyClass])
    assertSame(true, (sub: Any).isInstanceOf[Sub])
  }

  def testObject(): Unit = {
    assertSame(5, MyObject.foo(4))

    val obj1 = MyObject
    val obj2 = MyObject
    assertSame(obj1, obj2)
  }

  class MyClass(private var s: String) extends js.Object {
    var i: Int = _

    def foobar(x: Int): Int = x + 3

    def prop: String = "prop " + s
    def prop_=(v: String): Unit =
      s = "set " + v
  }

  class Sub extends MyClass("subarg") {
    override def foobar(x: Int): Int = super.foobar(x * 2)

    override def prop: String = super.prop + " sub"
    override def prop_=(v: String): Unit =
      super.prop_=("from sub " + v)
  }

  object MyObject extends js.Object {
    def foo(x: Int): Int = x + 1
  }
}
