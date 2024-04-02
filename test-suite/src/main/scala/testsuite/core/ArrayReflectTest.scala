package testsuite.core

import testsuite.Assert._

object ArrayReflectTest {
  def main(): Unit = {
    testSimpleNewInstance()
    testMultiNewInstance()
  }

  private def testSimpleNewInstance(): Unit = {
    val a = java.lang.reflect.Array.newInstance(classOf[Int], 5)
    ok(a.isInstanceOf[Array[Int]])
    assertSame(classOf[Array[Int]], a.getClass())
    val a1 = a.asInstanceOf[Array[Int]]
    assertSame(5, a1.length)
    assertSame(0, a1(0))

    val b = java.lang.reflect.Array.newInstance(classOf[String], 5)
    ok(b.isInstanceOf[Array[String]])
    assertSame(classOf[Array[String]], b.getClass())
    val b1 = b.asInstanceOf[Array[String]]
    assertSame(5, b1.length)
    assertSame(null, b1(0))

    val c = java.lang.reflect.Array.newInstance(classOf[Array[Object]], 5)
    ok(c.isInstanceOf[Array[Array[Object]]])
    assertSame(classOf[Array[Array[Object]]], c.getClass())
    val c1 = c.asInstanceOf[Array[Array[Object]]]
    assertSame(5, c1.length)
    assertSame(null, c1(0))
  }

  private def testMultiNewInstance(): Unit = {
    val a = java.lang.reflect.Array.newInstance(classOf[Int], 5, 10)
    ok(a.isInstanceOf[Array[Array[Int]]])
    assertSame(classOf[Array[Array[Int]]], a.getClass())
    val a1 = a.asInstanceOf[Array[Array[Int]]]
    assertSame(5, a1.length)
    ok(a1(3).isInstanceOf[Array[Int]])
    val a2 = a1(3).asInstanceOf[Array[Int]]
    assertSame(10, a2.length)
    assertSame(0, a2(0))
    ok(a1(1) ne a1(0))

    val b = java.lang.reflect.Array.newInstance(classOf[String], 5, 10)
    ok(b.isInstanceOf[Array[Array[String]]])
    assertSame(classOf[Array[Array[String]]], b.getClass())
    val b1 = b.asInstanceOf[Array[Array[String]]]
    assertSame(5, b1.length)
    ok(b1(3).isInstanceOf[Array[String]])
    val b2 = b1(3).asInstanceOf[Array[String]]
    assertSame(10, b2.length)
    assertSame(null, b2(0))
    ok(b1(1) ne b1(0))

    val c = java.lang.reflect.Array.newInstance(classOf[Array[Object]], 5, 10)
    ok(c.isInstanceOf[Array[Array[Array[Object]]]])
    assertSame(classOf[Array[Array[Array[Object]]]], c.getClass())
    val c1 = c.asInstanceOf[Array[Array[Array[Object]]]]
    assertSame(5, c1.length)
    ok(c1(3).isInstanceOf[Array[Array[Object]]])
    val c2 = c1(3).asInstanceOf[Array[Array[Object]]]
    assertSame(10, c2.length)
    assertSame(null, c2(0))
    ok(c1(1) ne c1(0))
  }
}
