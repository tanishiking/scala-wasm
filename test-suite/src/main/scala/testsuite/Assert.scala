package testsuite

/** Temporary assertion method on Scala for Wasm. `ok` method generates `unreachable` if the given
  * condition is false, trapping at runtime.
  *
  * While it's desirable to eventually utilize Scala's assertion, it's currently unavailable because
  * we cannot compile Throwable to wasm yet, thus throw new (Throwable) is unusable. and making
  * assert unavailable as well.
  *
  * Using JS's assert isn't feasible either; `console.assert` merely displays a message when
  * assertion failure, and Node's assert module is unsupported for Wasm due to current
  * unavailability of `JSImport` and module.
  */
object Assert {
  def ok(cond: Boolean): Unit =
    if (!cond) fail()

  def assertSame(expected: Any, actual: Any): Unit =
    ok(expected.asInstanceOf[AnyRef] eq actual.asInstanceOf[AnyRef])

  def fail(): Unit =
    null.toString() // Apply to Null should compile to unreachable
}
