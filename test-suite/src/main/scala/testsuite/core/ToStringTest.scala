package testsuite.core

import testsuite.Assert.ok

object ToStringTest {
  def main(): Unit = {
    ok(
      testBoolean(true, "true") &&
        testBoolean(false, "false") &&
        !testBoolean(true, "tru") && // confidence test
        testAny(true, "true") &&
        testAny(false, "false") &&
        testChar('A', "A") &&
        testAny('A', "A") &&
        testByte(54.toByte, "54") &&
        testAny(54.toByte, "54") &&
        testShort(6543.toShort, "6543") &&
        testAny(6543.toShort, "6543") &&
        testInt(-3423456, "-3423456") &&
        testAny(-3423456, "-3423456") &&
        testLong(1234567891011L, "1234567891011") &&
        testAny(1234567891011L, "1234567891011") &&
        testFloat(1.5f, "1.5") &&
        testAny(1.5f, "1.5") &&
        testDouble(1.4, "1.4") &&
        testAny(1.4, "1.4") &&
        testString("foo", "foo") &&
        testAny("foo", "foo") &&
        testString(null, "null") &&
        testAny(null, "null") &&
        testUndef((), "undefined") &&
        testAny((), "undefined") &&
        testMyToString(new MyToString(), "my toString") &&
        testMyToString(null, "null") &&
        testAny(new MyToString(), "my toString") &&
        testToStringNull(new ToStringNull(), "null") &&
        testToStringNull(null, "null") &&
        testAny(new ToStringNull(), "null") &&
        testConcat(1, "foo", "1foo") &&
        testConcat(2, null, "2null")
    )
  }

  def testBoolean(x: Boolean, expected: String): Boolean =
    "" + x == expected

  def testChar(x: Char, expected: String): Boolean =
    "" + x == expected

  def testByte(x: Byte, expected: String): Boolean =
    "" + x == expected

  def testShort(x: Short, expected: String): Boolean =
    "" + x == expected

  def testInt(x: Int, expected: String): Boolean =
    "" + x == expected

  def testLong(x: Long, expected: String): Boolean =
    "" + x == expected

  def testFloat(x: Float, expected: String): Boolean =
    "" + x == expected

  def testDouble(x: Double, expected: String): Boolean =
    "" + x == expected

  def testString(x: String, expected: String): Boolean =
    "" + x == expected

  def testUndef(x: Unit, expected: String): Boolean =
    "" + x == expected

  def testMyToString(x: MyToString, expected: String): Boolean =
    "" + x == expected

  def testToStringNull(x: ToStringNull, expected: String): Boolean =
    "" + x == expected

  def testAny(x: Any, expected: String): Boolean =
    "" + x == expected

  def testConcat(x: Int, y: String, expected: String): Boolean =
    "" + x + y == expected

  class MyToString {
    @noinline
    override def toString(): String = "my toString"
  }

  class ToStringNull {
    @noinline
    override def toString(): String = null // evil
  }
}
