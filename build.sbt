
lazy val wasm = project
  .in(file("wasm"))
  .settings(
    name := "wasm4s",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.13.12",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.scala-js" %%% "scalajs-linker" % "1.15.0"
    )
  )
//   .enablePlugins(ScalaJSPlugin)
