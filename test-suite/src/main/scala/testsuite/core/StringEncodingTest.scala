package testsuite.core

import testsuite.Assert._

object StringEncodingTest {
  def main(): Unit = {
    illformedUTF16Test()
    wellformedUTF16Test()
  }

  def illformedUTF16Test(): Unit = {
    val s = "\ud834a\udd1e"
    assertSame(3, s.length())
    assertSame(0xd834, s.charAt(0).toInt)
    assertSame('a'.toInt, s.charAt(1).toInt)
    assertSame(0xdd1e, s.charAt(2).toInt)
  }

  def wellformedUTF16Test(): Unit = {
    val s = "\ud834\udd1e"
    assertSame(2, s.length())
    assertSame(0xd834, s.charAt(0).toInt)
    assertSame(0xdd1e, s.charAt(1).toInt)
  }
}
