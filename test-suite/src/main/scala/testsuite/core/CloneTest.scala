package testsuite.core

import testsuite.Assert

object CloneTest {
  def main(): Unit = {
    testDirect()
    testIndirectlyImpl()
    testShallowCopyiny()
  }

  private def testDirect(): Unit = {
    val foo = new Foo(0, "0")
    val copy = foo.clone().asInstanceOf[Foo]
    Assert.ok(foo.x == copy.x)
    Assert.ok(foo.y == copy.y)
  }

  private def testIndirectlyImpl(): Unit = {
    val bar = new Bar(1, "1")
    val copy = bar.clone().asInstanceOf[Bar]
    Assert.ok(bar.x == copy.x)
    Assert.ok(bar.y == copy.y)
  }

  private def testShallowCopyiny(): Unit = {
    val foo = new Foo(0, "foo")
    val bar = new Bar(1, "bar")
    val baz = new Baz(foo, bar)
    val copy = baz.clone().asInstanceOf[Baz]
    Assert.assertSame(baz.foo, copy.foo)
    Assert.assertSame(baz.bar, copy.bar)
  }
}

class Foo(val x: Int, val y: String) extends Cloneable {
  override def clone(): Object = super.clone()
}

class Bar(
    override val x: Int,
    override val y: String
) extends Foo(x, y)

class Baz(val foo: Foo, val bar: Bar) extends Cloneable {
  override def clone(): Object = super.clone()
}
