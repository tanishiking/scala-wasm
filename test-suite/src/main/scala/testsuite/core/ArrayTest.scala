package testsuite.core

import testsuite.Assert

object ArrayTest {
  def main(): Unit = {
    Assert.ok(
      testLength() && testSelect() && testNew()
    )
  }

  def testLength(): Boolean = {
    Array(1, 2, 3).length == 3 &&
    (Array(Array(1, 2), Array(2), Array(3))).length == 3
  }

  def testSelect(): Boolean = {
    val a = Array(Array(1), Array(2), Array(3))
    a(0)(0) == 1 && {
      a(0)(0) = 100 // Assign(ArraySelect(...), ...)
      a(0)(0) == 100 // ArraySelect(...)
    } && {
      a(1) = Array(1, 2, 3)
      a(1).length == 3 && a(1)(0) == 1
    }
  }

  def testNew(): Boolean = {
    (Array.emptyBooleanArray.length == 0) &&
    (new Array[Int](10)).length == 10 &&
    (new Array[Int](1))(0) == 0 &&
    (new Array[Array[Array[Int]]](5))(0) == null
  }

  // TODO: Array.ofDim[T](...)
}
