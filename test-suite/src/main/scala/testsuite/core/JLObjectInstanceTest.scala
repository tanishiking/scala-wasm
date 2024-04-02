package testsuite.core

import testsuite.Assert._

object JLObjectInstanceTest {
  def main(): Unit = {
    val obj = new Object()
    val hashCode = obj.hashCode()
    val str = obj.toString()
    assertSame("java.lang.Object@" + Integer.toHexString(hashCode), str)

    assertSame(true, (obj: Any).isInstanceOf[Object])
    assertSame(false, (obj: Any).isInstanceOf[Comparable[_]])
  }
}
