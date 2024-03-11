package cli

object TestSuites {
  case class TestSuite(className: String, methodName: String)
  val suites = List(
    TestSuite("testsuite.core.simple.Simple", "simple"),
    TestSuite("testsuite.core.add.Add", "add"),
    TestSuite("testsuite.core.add.Add", "add"),
    TestSuite("testsuite.core.virtualdispatch.VirtualDispatch", "virtualDispatch"),
    TestSuite("testsuite.core.interfacecall.InterfaceCall", "interfaceCall"),
    TestSuite("testsuite.core.asinstanceof.AsInstanceOfTest", "asInstanceOf"),
    TestSuite("testsuite.core.hijackedclassesmono.HijackedClassesMonoTest", "hijackedClassesMono")
  )
}
