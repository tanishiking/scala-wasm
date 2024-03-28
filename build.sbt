import org.scalajs.linker.interface.ESVersion
import org.scalajs.linker.interface.OutputPatterns

val scalaV = "2.12.19"

inThisBuild(Def.settings(
  scalacOptions ++= Seq(
    "-encoding",
    "utf-8",
    "-feature",
    "-deprecation",
    "-Xfatal-warnings",
  ),
  scalaJSLinkerConfig ~= {
    _.withESFeatures(_.withESVersion(ESVersion.ES2016))
  },
))

lazy val cli = project
  .in(file("cli"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "cli",
    scalaVersion := scalaV,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule),
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1"
    ),
  )
  .dependsOn(
    wasm.js,
    // tests // for TestSuites constant
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
    ),
  )

lazy val sample = project
  .in(file("sample"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := scalaV,
    scalaJSUseMainModuleInitializer := true,
    Compile / jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      val cp = Attributed
        .data((Compile / fullClasspath).value)
        .mkString(";")
      val env = Map(
        "SCALAJS_CLASSPATH" -> cp,
        "SCALAJS_MODE" -> "sample",
      )
      new NodeJSEnv(NodeJSEnv.Config().withEnv(env).withArgs(List("--enable-source-maps")))
    },
    Compile / jsEnvInput := (`cli` / Compile / jsEnvInput).value
  )

lazy val testSuite = project
  .in(file("test-suite"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := scalaV,
    scalaJSUseMainModuleInitializer := true,
    Compile / jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      val cp = Attributed
        .data((Compile / fullClasspath).value)
        .mkString(";")
      val env = Map(
        "SCALAJS_CLASSPATH" -> cp,
        "SCALAJS_MODE" -> "testsuite",
      )
      new NodeJSEnv(NodeJSEnv.Config().withEnv(env).withArgs(List("--enable-source-maps")))
    },
    Compile / jsEnvInput := (`cli` / Compile / jsEnvInput).value
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
      // Generate CoreTests as an ES module so that it can import the loader.mjs
      // Give it an `.mjs` extension so that Node.js actually interprets it as an ES module
      _.withModuleKind(ModuleKind.ESModule)
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs")),
    },
    test := Def.sequential(
      (testSuite / Compile / run).toTask(""),
      (Test / test)
    ).value
  ).dependsOn(cli)
