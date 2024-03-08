package testsuite.core.tostring

import scala.scalajs.js.annotation._

object ToStringTest {
  def main(): Unit = { val _ = test() }

  @JSExportTopLevel("toStringConversions")
  def test(): Boolean = {
    testBoolean(true, "true") &&
      testBoolean(false, "false") &&
      !testBoolean(true, "tru") && // confidence test
      testChar('A', "A") &&
      testByte(54, "54") &&
      testShort(6543, "6543") &&
      testInt(-3423456, "-3423456") &&
      //testLong(1234567891011L, "1234567891011") && // TODO does not work yet
      testFloat(1.5f, "1.5") &&
      testDouble(1.4, "1.4") &&
      testString("foo", "foo") &&
      testString(null, "null") &&
      //testUndef((), "undefined") && // TODO does not work yet
      testConcat(1, "foo", "1foo") &&
      testConcat(2, null, "2null")
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

  def testConcat(x: Int, y: String, expected: String): Boolean =
    "" + x + y == expected
}
