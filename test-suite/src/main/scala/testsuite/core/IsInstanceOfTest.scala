package testsuite.core

import testsuite.Assert.ok

object IsInstanceOfTest {
  def main(): Unit = {
    ok(testInheritance())
    ok(testMixinAll(new Child()))
    ok(testMixinAll(new Parent()))
    ok(testMixinAll(new Base {}))
    ok(testMixin())
    ok(testPrimitiveIsInstanceOfBase(5))
    ok(testPrimitiveIsInstanceOfBase("foo"))

    ok(!testInt("foo"))
    ok(!testInt(2147483648L))
    ok(testInt(3))
    ok(!testInt(new Child()))
    ok(testString("foo"))
    ok(!testString(new Child()))
  }

  private def testInheritance(): Boolean = {
    val child: Any = new Child()
    val parent: Any = new Parent()
    val unrelated: Any = new UnrelatedClass()
    child.isInstanceOf[AbstractParent] &&
    child.isInstanceOf[Parent] &&
    child.isInstanceOf[Child] &&
    parent.isInstanceOf[AbstractParent] &&
    parent.isInstanceOf[Parent] &&
    !parent.isInstanceOf[Child] &&
    !unrelated.isInstanceOf[AbstractParent] &&
    !unrelated.isInstanceOf[Parent] &&
    !unrelated.isInstanceOf[Child]
  }

  private def testMixinAll(o: Base): Boolean = {
    o.isInstanceOf[Base] && o.isInstanceOf[Base1] && o.isInstanceOf[Base2]
  }

  private def testMixin(): Boolean = {
    val base1 = new Base1 {}
    val base2 = new Base2 {}
    base1.isInstanceOf[Base1] &&
    !base1.isInstanceOf[Base2] &&
    !base1.isInstanceOf[Base] &&
    !base2.isInstanceOf[Base1] &&
    base2.isInstanceOf[Base2] &&
    !base2.isInstanceOf[Base]
  }

  private def testPrimitiveIsInstanceOfBase(p: Any): Boolean =
    !p.isInstanceOf[Base]

  private def testInt(e: Any): Boolean = e.isInstanceOf[Int]
  private def testString(e: Any): Boolean = e.isInstanceOf[String]
}

abstract class AbstractParent

class Parent extends AbstractParent with Base {
  def foo(): Int = 5
}
class Child extends Parent

class UnrelatedClass

trait Base1
trait Base2
trait Base extends Base1 with Base2
