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

    testArrays()
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

  private def testArrays(): Unit = {
    val child: Any = new Child
    val booleanArray: Any = new Array[Boolean](1)
    val intArray: Any = new Array[Int](1)
    val stringArray: Any = new Array[String](1)
    val intArrayArray: Any = new Array[Array[Int]](1)
    val stringArrayArray: Any = new Array[Array[String]](1)
    val objectArray: Any = new Array[Object](1)

    // direct instances
    ok(booleanArray.isInstanceOf[Array[Boolean]])
    ok(intArray.isInstanceOf[Array[Int]])
    ok(stringArray.isInstanceOf[Array[String]])
    ok(intArrayArray.isInstanceOf[Array[Array[Int]]])
    ok(stringArrayArray.isInstanceOf[Array[Array[String]]])
    ok(objectArray.isInstanceOf[Array[Object]])

    // primitive arrays are not instances of any other type
    ok(!booleanArray.isInstanceOf[Array[Int]])
    ok(!booleanArray.isInstanceOf[Child])
    ok(!booleanArray.isInstanceOf[Array[Array[Boolean]]])
    ok(!booleanArray.isInstanceOf[Array[Object]])

    // different dimensions
    ok(!intArray.isInstanceOf[Array[Array[Int]]])
    ok(!intArrayArray.isInstanceOf[Array[Int]])
    ok(!intArrayArray.isInstanceOf[Array[Array[Array[Int]]]])
    ok(!stringArrayArray.isInstanceOf[Array[String]])
    ok(!stringArrayArray.isInstanceOf[Array[Array[Array[String]]]])
    ok(!objectArray.isInstanceOf[Array[Array[Object]]])

    // reference array types are covariant at run-time (IR and JVM semantics)
    ok(stringArray.isInstanceOf[Array[Object]])
    ok(intArrayArray.isInstanceOf[Array[Object]])
    ok(!objectArray.isInstanceOf[Array[String]])

    // non-arrays are not instances of any array type
    ok(!child.isInstanceOf[Array[Boolean]])
    ok(!child.isInstanceOf[Array[Int]])
    ok(!child.isInstanceOf[Array[String]])
    ok(!child.isInstanceOf[Array[Array[Int]]])
    ok(!child.isInstanceOf[Array[Array[String]]])
    ok(!child.isInstanceOf[Array[Object]])

    // corner case for reachability of the typeData
    val arrayOfClassUsedOnlyForArrayTypeTest = new Array[ClassUsedOnlyForArrayTypeTest](1)
    ok(arrayOfClassUsedOnlyForArrayTypeTest.isInstanceOf[Array[InterfaceUsedOnlyForArrayTypeTest]])
    ok(!intArray.isInstanceOf[Array[InterfaceUsedOnlyForArrayTypeTest]])
    ok(!objectArray.isInstanceOf[Array[InterfaceUsedOnlyForArrayTypeTest]])
  }

  private def testPrimitiveIsInstanceOfBase(p: Any): Boolean =
    !p.isInstanceOf[Base]

  private def testInt(e: Any): Boolean = e.isInstanceOf[Int]
  private def testString(e: Any): Boolean = e.isInstanceOf[String]

  trait InterfaceUsedOnlyForArrayTypeTest
  class ClassUsedOnlyForArrayTypeTest extends InterfaceUsedOnlyForArrayTypeTest
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
