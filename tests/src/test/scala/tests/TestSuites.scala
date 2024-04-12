package tests

import scala.scalajs.js

object TestSuites {
  case class TestSuite(
      className: String,
      methodName: String = "main",
      postLoadTests: Option[js.Dynamic => Unit] = None
  )

  val suites = List(
    TestSuite("testsuite.core.Simple"),
    TestSuite("testsuite.core.Add"),
    TestSuite("testsuite.core.ArrayTest"),
    TestSuite("testsuite.core.ArrayReflectTest"),
    TestSuite("testsuite.core.BasicListTest"),
    TestSuite("testsuite.core.CloneTest"),
    TestSuite("testsuite.core.ControlStructuresTest"),
    TestSuite("testsuite.core.VirtualDispatch"),
    TestSuite("testsuite.core.InterfaceCall"),
    TestSuite("testsuite.core.AsInstanceOfTest"),
    TestSuite("testsuite.core.IsInstanceOfTest"),
    TestSuite("testsuite.core.ClassOfTest"),
    TestSuite("testsuite.core.ClosureTest"),
    TestSuite("testsuite.core.FieldsTest"),
    TestSuite("testsuite.core.FloatingPointRemTest"),
    TestSuite("testsuite.core.GetClassTest"),
    TestSuite("testsuite.core.JLObjectInstanceTest"),
    TestSuite("testsuite.core.JSForInTest"),
    TestSuite("testsuite.core.JSImportCallTest"),
    TestSuite("testsuite.core.JSImportMetaTest"),
    TestSuite("testsuite.core.JSInteropTest"),
    TestSuite("testsuite.core.HijackedClassesDispatchTest"),
    TestSuite("testsuite.core.HijackedClassesMonoTest"),
    TestSuite("testsuite.core.HijackedClassesUpcastTest"),
    TestSuite("testsuite.core.ImportTest"),
    TestSuite("testsuite.core.NonNativeJSClassTest"),
    TestSuite("testsuite.core.StaticFieldsTest"),
    TestSuite("testsuite.core.StaticMethodTest"),
    TestSuite("testsuite.core.ThrowAndTryTest"),
    TestSuite("testsuite.core.ThrowablesTest"),
    TestSuite("testsuite.core.ToStringTest"),
    TestSuite("testsuite.core.UnitPatMatTest"),
    TestSuite("testsuite.core.MatchTest"),
    TestSuite("testsuite.core.WrapUnwrapThrowableTest"),
    TestSuite("testsuite.core.StringEncodingTest"),
    TestSuite("testsuite.core.ReflectiveCallTest"),
    TestSuite(
      "testsuite.core.JSExportTopLevelTest",
      postLoadTests = Some({ moduleExports =>
        import munit.Assertions.assert

        assert((moduleExports.immutableField: Any) == "my immutable field value")
        assert((moduleExports.mutableField: Any) == 42)
        assert((moduleExports.simpleFunction(5): Any) == 25)
        assert((moduleExports.functionWithRest(3, 5, 6, 7): Any) == 54)
        assert((moduleExports.SimpleObject.bar: Any) == "the bar field")

        val obj = js.Dynamic.newInstance(moduleExports.SimpleClass)(456)
        assert((obj.foo: Any) == 456)
      })
    )
  )
}
