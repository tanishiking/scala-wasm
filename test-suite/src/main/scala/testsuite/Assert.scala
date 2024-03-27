package testsuite

/** Assertion helpers. */
object Assert {
  def ok(cond: Boolean): Unit =
    if (!cond) fail()

  def assertSame(expected: Any, actual: Any): Unit =
    ok(expected.asInstanceOf[AnyRef] eq actual.asInstanceOf[AnyRef])

  def fail(): Unit =
    throw new AssertionError("assertion failed")
}
