val scalaV = "2.13.12"

lazy val cli = project
  .in(file("cli"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "cli",
    scalaVersion := scalaV,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule),
    }
  )
  .dependsOn(wasm)

lazy val wasm = project
  .in(file("wasm"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "wasm",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.scala-js" %%% "scalajs-linker" % "1.15.0"
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule),
    }
  )
//   .enablePlugins(ScalaJSPlugin)

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
        // .filter { path =>
        //     val pathStr = path.toString()
        //     println(pathStr)
        //     pathStr.contains("sample/target")
        // }
        .mkString(";")
      val env = Map("SCALAJS_CLASSPATH" -> cp, "SCALAJS_MODE" -> "sample")
      new NodeJSEnv(NodeJSEnv.Config().withEnv(env).withArgs(List("--enable-source-maps")))
    },
    Compile / jsEnvInput := (`cli` / Compile / jsEnvInput).value
  )
