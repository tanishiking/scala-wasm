package testsuite.core

import testsuite.Assert._

object BasicListTest {
  def main(): Unit = {
    val l = List(5, 13, 29)
    val x = l.tail.head
    assertSame(13, x)

    val l2 = l.map(x => x * 2)
    assertSame(3, l2.size)
    assertSame(26, l2(1))
  }
}
