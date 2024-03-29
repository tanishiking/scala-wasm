package testsuite.core

import testsuite.Assert.assertSame

object GetClassTest {
  def main(): Unit = {
    testNoHijackedDispatch()
    testWithHijackedDispath()
  }

  class Foo

  class Bar extends Foo

  def testNoHijackedDispatch(): Unit = {
    def getClassOfFoo(x: Foo): Class[_] = x.getClass()

    assertSame(classOf[Foo], getClassOfFoo(new Foo))
    assertSame(classOf[Bar], getClassOfFoo(new Bar))
  }

  def testWithHijackedDispath(): Unit = {
    def getClassOf(x: Any): Class[_] = x.getClass()

    assertSame(classOf[Foo], getClassOf(new Foo))
    assertSame(classOf[Bar], getClassOf(new Bar))

    assertSame(classOf[java.lang.Boolean], getClassOf(true))
    assertSame(classOf[java.lang.Boolean], getClassOf(false))
    assertSame(classOf[java.lang.Void], getClassOf(()))
    assertSame(classOf[java.lang.String], getClassOf("foo"))

    assertSame(classOf[java.lang.Byte], getClassOf(0.0))
    assertSame(classOf[java.lang.Byte], getClassOf(56))
    assertSame(classOf[java.lang.Byte], getClassOf(-128))
    assertSame(classOf[java.lang.Short], getClassOf(200))
    assertSame(classOf[java.lang.Short], getClassOf(-32000))
    assertSame(classOf[java.lang.Integer], getClassOf(500000))
    assertSame(classOf[java.lang.Integer], getClassOf(Int.MinValue))

    assertSame(classOf[java.lang.Float], getClassOf(1.5))
    assertSame(classOf[java.lang.Double], getClassOf(1.4))
    assertSame(classOf[java.lang.Double], getClassOf(Float.MaxValue.toDouble * 8.0))

    assertSame(classOf[java.lang.Float], getClassOf(-0.0))
    assertSame(classOf[java.lang.Float], getClassOf(Double.PositiveInfinity))
    assertSame(classOf[java.lang.Float], getClassOf(Double.NegativeInfinity))
    assertSame(classOf[java.lang.Float], getClassOf(Double.NaN))

    assertSame(classOf[Array[Int]], getClassOf(new Array[Int](3)))
    assertSame(classOf[Array[Int]], getClassOf(Array(1, 2, 3)))

    assertSame(classOf[Array[Boolean]], getClassOf(new Array[Boolean](3)))
    assertSame(classOf[Array[Boolean]], getClassOf(Array(false, true)))

    assertSame(classOf[Array[AnyRef]], getClassOf(new Array[AnyRef](3)))
    assertSame(classOf[Array[AnyRef]], getClassOf(Array(null): Array[AnyRef]))

    assertSame(classOf[Array[String]], getClassOf(new Array[String](3)))
    assertSame(classOf[Array[String]], getClassOf(Array("foo", "bar")))

    assertSame(classOf[Array[Array[Int]]], getClassOf(new Array[Array[Int]](3)))
    assertSame(classOf[Array[Array[Int]]], getClassOf(Array(Array(1, 2, 3))))

    assertSame(null, getClassOf(scala.scalajs.js.Math))
  }
}
