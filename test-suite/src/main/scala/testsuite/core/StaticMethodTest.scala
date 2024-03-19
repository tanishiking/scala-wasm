package testsuite.core

import testsuite.Assert.ok

object StaticMethodTest {
  def main(): Unit = {
    ok(java.lang.Integer.sum(5, 65) == 70)
    ok(java.lang.Integer.reverseBytes(0x01020304) == 0x04030201)
  }
}
