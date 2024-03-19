package cli

object TestSuites {
  case class TestSuite(className: String, methodName: String = "main")
  val suites = List(
    TestSuite("testsuite.core.Simple"),
    TestSuite("testsuite.core.Add"),
    TestSuite("testsuite.core.VirtualDispatch"),
    TestSuite("testsuite.core.InterfaceCall"),
    TestSuite("testsuite.core.AsInstanceOfTest"),
    TestSuite("testsuite.core.ClassOfTest"),
    TestSuite("testsuite.core.GetClassTest"),
    TestSuite("testsuite.core.JSInteropTest"),
    TestSuite("testsuite.core.HijackedClassesDispatchTest"),
    TestSuite("testsuite.core.HijackedClassesMonoTest"),
    TestSuite("testsuite.core.HijackedClassesUpcastTest"),
    TestSuite("testsuite.core.StaticMethodTest"),
    TestSuite("testsuite.core.ToStringTest")
  )
}
