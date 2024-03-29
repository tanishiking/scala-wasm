package cli

object TestSuites {
  case class TestSuite(className: String, methodName: String = "main")
  val suites = List(
    TestSuite("testsuite.core.Simple"),
    TestSuite("testsuite.core.Add"),
    TestSuite("testsuite.core.ArrayTest"),
    TestSuite("testsuite.core.CloneTest"),
    TestSuite("testsuite.core.ControlStructuresTest"),
    TestSuite("testsuite.core.VirtualDispatch"),
    TestSuite("testsuite.core.InterfaceCall"),
    TestSuite("testsuite.core.AsInstanceOfTest"),
    TestSuite("testsuite.core.IsInstanceOfTest"),
    TestSuite("testsuite.core.ClassOfTest"),
    TestSuite("testsuite.core.ClosureTest"),
    TestSuite("testsuite.core.FieldsTest"),
    TestSuite("testsuite.core.GetClassTest"),
    TestSuite("testsuite.core.JSInteropTest"),
    TestSuite("testsuite.core.HijackedClassesDispatchTest"),
    TestSuite("testsuite.core.HijackedClassesMonoTest"),
    TestSuite("testsuite.core.HijackedClassesUpcastTest"),
    TestSuite("testsuite.core.StaticMethodTest"),
    TestSuite("testsuite.core.ThrowablesTest"),
    TestSuite("testsuite.core.ToStringTest"),
    TestSuite("testsuite.core.WrapUnwrapThrowableTest")
  )
}
