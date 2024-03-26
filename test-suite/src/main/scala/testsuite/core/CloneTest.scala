package testsuite.core

import testsuite.Assert

object CloneTest {
  def main(): Unit = {
    testDirect()
    testIndirectlyImpl()
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
}

class Foo(val x: Int, val y: String) extends Cloneable {
  override def clone(): Object = super.clone()
}

class Bar(
    override val x: Int,
    override val y: String
) extends Foo(x, y)
