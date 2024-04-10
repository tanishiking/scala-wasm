import org.scalajs.jsenv.nodejs.NodeJSEnv

import org.scalajs.linker.interface.ESVersion
import org.scalajs.linker.interface.OutputPatterns

val scalaV = "2.12.19"

val fetchScalaJSSource = taskKey[File]("Fetches the source code of Scala.js")

val writePackageJSON = taskKey[Unit](
  "Write package.json to configure module type for Node.js"
)

// Include wasm.jvm on the classpath used to dynamically load Scala.js linkers
Global / scalaJSLinkerImpl / fullClasspath :=
  (wasm.jvm / Compile / fullClasspath).value

inThisBuild(
  Def.settings(
    scalacOptions ++= Seq(
      "-encoding",
      "utf-8",
      "-feature",
      "-deprecation",
      "-Xfatal-warnings"
    ),
    scalaJSLinkerConfig ~= {
      _.withESFeatures(_.withESVersion(ESVersion.ES2016))
    },
    jsEnv := {
      // Enable support for exnref and try_table
      new NodeJSEnv(
        NodeJSEnv.Config().withArgs(List("--enable-source-maps", "--experimental-wasm-exnref"))
      )
    }
  )
)

lazy val wasm = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("wasm"))
  .settings(
    name := "wasm",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-linker" % "1.16.0"
    )
  )

lazy val sample = project
  .in(file("sample"))
  .enablePlugins(WasmLinkerPlugin, ScalaJSJUnitPlugin)
  .settings(
    scalaVersion := scalaV,
    scalaJSUseMainModuleInitializer := true,
    // Emit .wat files for exploratory and debugging purposes
    scalaJSLinkerConfig ~= { _.withPrettyPrint(true) },
  )

lazy val testSuite = project
  .in(file("test-suite"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := scalaV,
    scalaJSUseMainModuleInitializer := true,
    scalacOptions -= "-Xfatal-warnings", // for unpaired surrogate code units in StringEncodingTest.scala
  )

lazy val tests = project
  .in(file("tests"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
      "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1" % Test
    ),
    scalaJSLinkerConfig ~= {
      // Generate CoreTests as an ES module so that it can import the main.mjs files
      // Give it an `.mjs` extension so that Node.js actually interprets it as an ES module
      _.withModuleKind(ModuleKind.ESModule)
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs")),
    },
    envVars += {
      val testSuiteClasspath = Attributed.data((testSuite / Compile / fullClasspath).value)
      "SCALAJS_CLASSPATH" -> testSuiteClasspath.mkString(";")
    },
  )
  .dependsOn(wasm.js)

lazy val `scalajs-test-suite` = project
  .in(file("scalajs-test-suite"))
  .enablePlugins(WasmLinkerPlugin, ScalaJSJUnitPlugin)
  .settings(
    fetchScalaJSSource / artifactPath :=
      baseDirectory.value / "fetched-sources" / scalaJSVersion,

    fetchScalaJSSource := {
      import org.eclipse.jgit.api._

      val s = streams.value
      val ver = scalaJSVersion
      val trgDir = (fetchScalaJSSource / artifactPath).value

      if (!trgDir.exists) {
        s.log.info(s"Fetching Scala.js source version $ver")

        // Make parent dirs
        IO.createDirectory(trgDir)

        // Clone Scala.js source code
        new CloneCommand()
          .setDirectory(trgDir)
          .setURI("https://github.com/scala-js/scala-js.git")
          .call()
      }

      // Checkout the proper ref. We do this anyway so we fail if something is wrong.
      val git = Git.open(trgDir)
      s.log.info(s"Checking out Scala.js source version $ver")
      git.checkout().setName(s"v$ver").call()

      trgDir
    },

    // We need them in `main` as well as in `test`
    libraryDependencies += "org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion,

    scalacOptions --= Seq("-deprecation", "-Xfatal-warnings"),

    Test / unmanagedResourceDirectories ++= {
      val base = (fetchScalaJSSource / artifactPath).value
      val testDir = base / "test-suite/js/src/test"

      scalaJSLinkerConfig.value.moduleKind match {
        case ModuleKind.NoModule       => Nil
        case ModuleKind.CommonJSModule => Seq(testDir / "resources-commonjs")
        case ModuleKind.ESModule       => Seq(testDir / "resources-esmodule")
      }
    },

    Compile / unmanagedSourceDirectories ++= {
      val base = (fetchScalaJSSource / artifactPath).value
      Seq(
        base / "junit-async/js/src/main/scala",
        base / "test-suite/shared/src/main/scala",
        base / "test-suite/js/src/main/scala",
      )
    },

    Compile / unmanagedSources := (Compile / unmanagedSources).dependsOn(fetchScalaJSSource).value,
    Test / unmanagedSources := (Test / unmanagedSources).dependsOn(fetchScalaJSSource).value,

    Compile / sources ~= { sources =>
      sources
        .filter(_.getName != "TypecheckingMacros.scala")
        .filter(_.getName != "Typechecking.scala")
    },

    Test / unmanagedSourceDirectories ++= {
      val base = (fetchScalaJSSource / artifactPath).value
      Seq(
        base / "test-suite/shared/src/test/scala/",
        base / "test-suite/shared/src/test/require-scala2/",
        base / "test-suite/shared/src/test/scala-old-collections",

        base / "test-suite/js/src/test/require-dynamic-import",
        base / "test-suite/js/src/test/esmodule",
        base / "test-suite/js/src/test/require-exponent-op",
        base / "test-suite/js/src/test/require-modules",
        base / "test-suite/js/src/test/require-new-target",
        base / "test-suite/js/src/test/require-scala2",
        base / "test-suite/js/src/test/scala",
        base / "test-suite/js/src/test/scala-old-collections",
      )
    },

    // Remove sources of tests that cause linking/validating to fail
    Test / sources := {
      def endsWith(f: File, suffix: String): Boolean =
        f.getPath().replace('\\', '/').endsWith(suffix)

      (Test / sources).value
        .filterNot(endsWith(_, "/UnionTypeTest.scala")) // requires typechecking macros
        .filterNot(endsWith(_, "/compiler/DefaultMethodsTest.scala")) // reflective calls
        .filterNot(endsWith(_, "/compiler/ReflectiveCallTest.scala")) // reflective calls
        .filterNot(endsWith(_, "/compiler/WasPublicBeforeTyperTestScala2.scala")) // reflective calls
        .filterNot(endsWith(_, "/jsinterop/ExportsTest.scala")) // js.dynamicImport (multi-modules)
    },

    Test / scalacOptions += "-P:scalajs:genStaticForwardersForNonTopLevelObjects",
    Test / scalacOptions += "-P:scalajs:nowarnGlobalExecutionContext",

    scalaJSLinkerConfig ~= { _.withSemantics(build.TestSuiteLinkerOptions.semantics _) },
    Test / scalaJSModuleInitializers ++= build.TestSuiteLinkerOptions.moduleInitializers,

    scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2016)) },

    Test / jsEnvInput := {
      val base = fetchScalaJSSource.value
      val f = (base / "test-suite/js/src/test/resources/NonNativeJSTypeTestNatives.js").toPath
      val resourcesInput = org.scalajs.jsenv.Input.Script(f)

      resourcesInput +: (Test / jsEnvInput).value
    },

    // Write package.json inside target/ to automatically configure the right module type
    Compile / jsEnvInput := (Compile / jsEnvInput).dependsOn(writePackageJSON).value,
    Test / jsEnvInput := (Test / jsEnvInput).dependsOn(writePackageJSON).value,
    writePackageJSON := {
      val packageType = scalaJSLinkerConfig.value.moduleKind match {
        case ModuleKind.NoModule       => "commonjs"
        case ModuleKind.CommonJSModule => "commonjs"
        case ModuleKind.ESModule       => "module"
      }
      val path = target.value / "package.json"
      IO.write(path, s"""{"type": "$packageType"}\n""")
    },

    Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),

    // Ignore individual tests that do not pass
    Test / testOptions += Tests.Filter({ testName =>
      !IgnoredTestNames.contains(testName)
    }),
  )

lazy val IgnoredTestNames: Set[String] = {
  Set(
    // javaLangNumber failed: java.lang.AssertionError: 1, class java.lang.Number expected:<true> but was:<false>
    "org.scalajs.testsuite.compiler.RuntimeTypeTestsTest",
    // Various run-time errors and JS exceptions
    "org.scalajs.testsuite.compiler.InteroperabilityTest",
    "org.scalajs.testsuite.compiler.RegressionJSTest",
    "org.scalajs.testsuite.jsinterop.FunctionTest",
    "org.scalajs.testsuite.jsinterop.MiscInteropTest",
    "org.scalajs.testsuite.jsinterop.NonNativeJSTypeTest",
    "org.scalajs.testsuite.jsinterop.SpecialTest",
    "org.scalajs.testsuite.jsinterop.SymbolTest",
    // RuntimeError: unreachable (in the `isInstance` helper)
    "org.scalajs.testsuite.compiler.ReflectionTest",
    "org.scalajs.testsuite.compiler.RuntimeTypeTestsJSTest",
    "org.scalajs.testsuite.jsinterop.ModulesTest",
    // getClass for Box classes
    "org.scalajs.testsuite.javalib.lang.ClassTest",
    "org.scalajs.testsuite.javalib.lang.ObjectTest",
    // eqEqJLFloat/eqEqJLDouble failed: java.lang.AssertionError: null
    "org.scalajs.testsuite.compiler.RegressionTest",
    // hashCode of floats and doubles
    "org.scalajs.testsuite.javalib.lang.DoubleTest",
    "org.scalajs.testsuite.javalib.lang.FloatTest",
    "org.scalajs.testsuite.javalib.util.ArraysTest",
    "org.scalajs.testsuite.typedarray.ArraysTest",
    // hashCode of bigints and symbols
    "org.scalajs.testsuite.javalib.lang.ObjectJSTest",
    // Various issues with identityHashCode
    "org.scalajs.testsuite.javalib.lang.SystemTest",
    // TypeError: WebAssembly objects are opaque
    "org.scalajs.testsuite.javalib.lang.SystemJSTest",
    // throwablesAreTrueErrors failed: org.junit.ComparisonFailure: expected:<[object [Error]]> but was:<[object [Object]]>
    // throwablesAreJSErrors failed: java.lang.AssertionError: null
    "org.scalajs.testsuite.javalib.lang.ThrowableJSTest",
    // keepBreakToLabelWithinFinallyBlock_Issue2689 failed: java.lang.AssertionError: expected:<2> but was:<1>
    "org.scalajs.testsuite.compiler.OptimizerTest",
    // nonUnitBoxedPrimitiveValuesAreSerializable failed: java.lang.AssertionError: Boolean
    "org.scalajs.testsuite.javalib.io.SerializableTest",
    // No support for stack traces
    "org.scalajs.testsuite.library.StackTraceTest",
  )
}
