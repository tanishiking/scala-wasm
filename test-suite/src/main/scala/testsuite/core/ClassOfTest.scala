package testsuite.core

import java.io.Serializable
import java.lang.Cloneable

import testsuite.Assert._
import testsuite.Assert

object ClassOfTest {
  def main(): Unit = {
    testGetName()
    testUniqueness()
    testIsPrimitive()
    testIsInterface()
    testIsArray()
    testIsAssignableFrom()
    testGetComponentType()
  }

  def testGetName(): Unit = {
    Assert.assertSame("java.lang.String", classOf[String].getName())
    Assert.assertSame("java.lang.StringBuilder", classOf[java.lang.StringBuilder].getName())
    Assert.assertSame("int", classOf[Int].getName())
    Assert.assertSame("void", classOf[Unit].getName())

    Assert.assertSame("[Ljava.lang.Object;", classOf[Array[AnyRef]].getName())
    Assert.assertSame("[Ljava.lang.String;", classOf[Array[String]].getName())
    Assert.assertSame("[[Ljava.lang.CharSequence;", classOf[Array[Array[CharSequence]]].getName())

    Assert.assertSame("[Z", classOf[Array[Boolean]].getName())
    Assert.assertSame("[C", classOf[Array[Char]].getName())
    Assert.assertSame("[B", classOf[Array[Byte]].getName())
    Assert.assertSame("[S", classOf[Array[Short]].getName())
    Assert.assertSame("[I", classOf[Array[Int]].getName())
    Assert.assertSame("[J", classOf[Array[Long]].getName())
    Assert.assertSame("[F", classOf[Array[Float]].getName())
    Assert.assertSame("[D", classOf[Array[Double]].getName())
  }

  def testUniqueness(): Unit = {
    Assert.assertSame(classOf[String], classOf[String])
    Assert.assertSame(classOf[java.lang.StringBuilder], classOf[java.lang.StringBuilder])
    Assert.assertSame(classOf[CharSequence], classOf[CharSequence])
    Assert.assertSame(classOf[Int], classOf[Int])

    Assert.assertSame(classOf[Array[Int]], classOf[Array[Int]])
    Assert.assertSame(classOf[Array[Array[java.lang.Byte]]], classOf[Array[Array[java.lang.Byte]]])
    Assert.assertSame(classOf[Array[Array[Int]]], classOf[Array[Array[Int]]])
  }

  def testIsPrimitive(): Unit = {
    Assert.assertSame(false, classOf[AnyRef].isPrimitive())
    Assert.assertSame(false, classOf[String].isPrimitive())
    Assert.assertSame(false, classOf[CharSequence].isPrimitive())
    Assert.assertSame(false, classOf[Cloneable].isPrimitive())
    Assert.assertSame(false, classOf[Serializable].isPrimitive())
    Assert.assertSame(false, classOf[java.lang.Iterable[Any]].isPrimitive())
    Assert.assertSame(false, classOf[Array[Int]].isPrimitive())
    Assert.assertSame(false, classOf[Array[String]].isPrimitive())
    Assert.assertSame(false, classOf[Array[CharSequence]].isPrimitive())

    Assert.assertSame(true, classOf[Int].isPrimitive())
    Assert.assertSame(true, classOf[Unit].isPrimitive())
  }

  def testIsInterface(): Unit = {
    Assert.assertSame(false, classOf[AnyRef].isInterface())
    Assert.assertSame(false, classOf[String].isInterface())
    Assert.assertSame(false, classOf[Int].isInterface())
    Assert.assertSame(false, classOf[Array[Int]].isInterface())
    Assert.assertSame(false, classOf[Array[String]].isInterface())
    Assert.assertSame(false, classOf[Array[CharSequence]].isInterface())

    Assert.assertSame(true, classOf[CharSequence].isInterface())
    Assert.assertSame(true, classOf[Cloneable].isInterface())
    Assert.assertSame(true, classOf[Serializable].isInterface())
    Assert.assertSame(true, classOf[java.lang.Iterable[Any]].isInterface())
  }

  def testIsArray(): Unit = {
    Assert.assertSame(false, classOf[AnyRef].isArray())
    Assert.assertSame(false, classOf[String].isArray())
    Assert.assertSame(false, classOf[Int].isArray())
    Assert.assertSame(false, classOf[CharSequence].isArray())
    Assert.assertSame(false, classOf[Cloneable].isArray())
    Assert.assertSame(false, classOf[Serializable].isArray())
    Assert.assertSame(false, classOf[java.lang.Iterable[Any]].isArray())

    Assert.assertSame(true, classOf[Array[Int]].isArray())
    Assert.assertSame(true, classOf[Array[String]].isArray())
    Assert.assertSame(true, classOf[Array[CharSequence]].isArray())
  }

  def testIsAssignableFrom(): Unit = {
    def testEquiv(cls1: Class[_], cls2: Class[_]): Unit = {
      assertSame(true, cls1.isAssignableFrom(cls2))
      assertSame(true, cls2.isAssignableFrom(cls1))
    }

    def testStrictSuper(cls1: Class[_], cls2: Class[_]): Unit = {
      assertSame(true, cls1.isAssignableFrom(cls2))
      assertSame(false, cls2.isAssignableFrom(cls1))
    }

    def testUnrelated(cls1: Class[_], cls2: Class[_]): Unit = {
      assertSame(false, cls1.isAssignableFrom(cls2))
      assertSame(false, cls2.isAssignableFrom(cls1))
    }

    // Same class is always assignable, including primitives and even void
    testEquiv(classOf[Int], classOf[Int])
    testEquiv(classOf[Unit], classOf[Unit])
    testEquiv(classOf[Object], classOf[Object])
    testEquiv(classOf[String], classOf[String])
    testEquiv(classOf[Comparable[_]], classOf[Comparable[_]])
    testEquiv(classOf[Array[Int]], classOf[Array[Int]])
    testEquiv(classOf[Interface], classOf[Interface])
    testEquiv(classOf[OtherInterface], classOf[OtherInterface])
    testEquiv(classOf[Parent], classOf[Parent])
    testEquiv(classOf[Child], classOf[Child])

    // Primitives are not assignable to/from anything elsething else
    testUnrelated(classOf[Int], classOf[Unit])
    testUnrelated(classOf[Int], classOf[Double])
    testUnrelated(classOf[Int], classOf[Object])
    testUnrelated(classOf[Int], classOf[String])
    testUnrelated(classOf[Int], classOf[Comparable[_]])
    testUnrelated(classOf[Int], classOf[Cloneable])
    testUnrelated(classOf[Int], classOf[Serializable])

    // Everything non-primitive is assignable to Object, including arrays
    testStrictSuper(classOf[Object], classOf[String])
    testStrictSuper(classOf[Object], classOf[Cloneable])
    testStrictSuper(classOf[Object], classOf[Serializable])
    testStrictSuper(classOf[Object], classOf[Comparable[_]])
    testStrictSuper(classOf[Object], classOf[Array[Int]])
    testStrictSuper(classOf[Object], classOf[Array[Array[String]]])

    // Subclasses are assignable to superclasses
    testStrictSuper(classOf[Interface], classOf[OtherInterface])
    testStrictSuper(classOf[Interface], classOf[Parent])
    testStrictSuper(classOf[Interface], classOf[Child])
    testStrictSuper(classOf[OtherInterface], classOf[Child])
    testStrictSuper(classOf[Parent], classOf[Child])

    // Unrelated classes are not assignable
    testUnrelated(classOf[String], classOf[Parent])
    testUnrelated(classOf[Parent], classOf[OtherInterface])
    testUnrelated(classOf[Parent], classOf[Cloneable])
    testUnrelated(classOf[Parent], classOf[Serializable])
    testUnrelated(classOf[Comparable[_]], classOf[Cloneable])
    testUnrelated(classOf[Comparable[_]], classOf[Serializable])
    testUnrelated(classOf[Comparable[_]], classOf[Parent])

    // Arrays are covariant
    testStrictSuper(classOf[Array[Object]], classOf[Array[String]])
    testStrictSuper(classOf[Array[Object]], classOf[Array[Interface]])
    testStrictSuper(classOf[Array[Object]], classOf[Array[Array[Int]]])
    testStrictSuper(classOf[Array[Interface]], classOf[Array[Parent]])
    testStrictSuper(classOf[Array[Parent]], classOf[Array[Child]])

    // Arrays are Cloneable, including covariantly
    testStrictSuper(classOf[Cloneable], classOf[Array[Int]])
    testStrictSuper(classOf[Cloneable], classOf[Array[Object]])
    testStrictSuper(classOf[Cloneable], classOf[Array[String]])
    testStrictSuper(classOf[Cloneable], classOf[Array[Array[Int]]])
    testStrictSuper(classOf[Cloneable], classOf[Array[Array[Object]]])
    testStrictSuper(classOf[Array[Cloneable]], classOf[Array[Array[Int]]])
    testStrictSuper(classOf[Array[Cloneable]], classOf[Array[Array[Array[String]]]])
    testUnrelated(classOf[Array[Cloneable]], classOf[Array[Int]])

    // Arrays are Serializable, including covariantly
    testStrictSuper(classOf[Serializable], classOf[Array[Int]])
    testStrictSuper(classOf[Serializable], classOf[Array[Object]])
    testStrictSuper(classOf[Serializable], classOf[Array[String]])
    testStrictSuper(classOf[Serializable], classOf[Array[Array[Int]]])
    testStrictSuper(classOf[Serializable], classOf[Array[Array[Object]]])
    testStrictSuper(classOf[Array[Serializable]], classOf[Array[Array[Int]]])
    testStrictSuper(classOf[Array[Serializable]], classOf[Array[Array[Array[String]]]])
    testUnrelated(classOf[Array[Serializable]], classOf[Array[Int]])

    // Arrays are unrelated to other interfaces
    testUnrelated(classOf[Interface], classOf[Array[Int]])
    testUnrelated(classOf[Interface], classOf[Array[Object]])
    testUnrelated(classOf[Interface], classOf[Array[String]])
    testUnrelated(classOf[Interface], classOf[Array[Interface]])
    testUnrelated(classOf[Interface], classOf[Array[Array[Int]]])
    testUnrelated(classOf[Interface], classOf[Array[Array[Object]]])
  }

  def testGetComponentType(): Unit = {
    Assert.assertSame(null, classOf[AnyRef].getComponentType())
    Assert.assertSame(null, classOf[String].getComponentType())
    Assert.assertSame(null, classOf[Int].getComponentType())
    Assert.assertSame(null, classOf[CharSequence].getComponentType())
    Assert.assertSame(null, classOf[java.lang.Iterable[Any]].getComponentType())

    Assert.assertSame(classOf[Int], classOf[Array[Int]].getComponentType())
    Assert.assertSame(classOf[String], classOf[Array[String]].getComponentType())
    Assert.assertSame(classOf[AnyRef], classOf[Array[AnyRef]].getComponentType())

    Assert.assertSame(
      classOf[Array[CharSequence]],
      classOf[Array[Array[CharSequence]]].getComponentType()
    )

    Assert.assertSame(
      classOf[Array[Long]],
      classOf[Array[Array[Long]]].getComponentType()
    )

    Assert.assertSame(
      classOf[Array[Array[java.lang.Byte]]],
      classOf[Array[Array[Array[java.lang.Byte]]]].getComponentType()
    )

    Assert.assertSame(
      classOf[Array[ClassForUniqueGetComponentTypeTest]].getComponentType(),
      classOf[Array[ClassForUniqueGetComponentTypeTest]].getComponentType()
    )
  }

  trait Interface
  trait OtherInterface extends Interface
  class Parent extends Interface
  class Child extends Parent with OtherInterface

  class ClassForUniqueGetComponentTypeTest
}
