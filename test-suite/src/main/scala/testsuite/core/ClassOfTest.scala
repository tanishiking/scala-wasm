package testsuite.core

import testsuite.Assert.ok
import testsuite.Assert

object ClassOfTest {
  def main(): Unit = {
    testGetName()
    testUniqueness()
    testIsPrimitive()
    testIsInterface()
    testIsArray()
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
    Assert.assertSame(true, classOf[java.lang.Iterable[Any]].isInterface())
  }

  def testIsArray(): Unit = {
    Assert.assertSame(false, classOf[AnyRef].isArray())
    Assert.assertSame(false, classOf[String].isArray())
    Assert.assertSame(false, classOf[Int].isArray())
    Assert.assertSame(false, classOf[CharSequence].isArray())
    Assert.assertSame(false, classOf[java.lang.Iterable[Any]].isArray())

    Assert.assertSame(true, classOf[Array[Int]].isArray())
    Assert.assertSame(true, classOf[Array[String]].isArray())
    Assert.assertSame(true, classOf[Array[CharSequence]].isArray())
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

  class ClassForUniqueGetComponentTypeTest
}
