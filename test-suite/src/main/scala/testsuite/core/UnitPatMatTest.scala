package testsuite.core

import testsuite.Assert._

object UnitPatMatTest {
  def main(): Unit = {
    assertEquals("failure", 5, 5)
  }

  @noinline
  def assertEquals(message: String, expected: Any, actual: Any): Unit = {
    if (!java.util.Objects.equals(expected, actual)) {
      (expected, actual) match {
        case (expectedString: String, actualString: String) =>
          val cleanMsg: String = if (message == null) "" else message
          throw new AssertionError(cleanMsg)

        case _ =>
          failNotEquals(message, expected, actual)
      }
    }
  }

  @noinline
  def failNotEquals(message: String, expected: Any, actual: Any): Unit =
    throw new AssertionError(message)
}
