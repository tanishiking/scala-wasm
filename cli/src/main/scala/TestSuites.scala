package cli

object TestSuites {
  case class TestSuite(className: String, methodName: String)
  val suites = List(
    TestSuite("testsuite.core.simple.Simple", "simple"),
    TestSuite("testsuite.core.add.Add", "add")
  )
}
