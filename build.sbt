val scala3Version = "3.0.0-M1"


lazy val mat = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "mat",
    version := "0.1.0",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-indent"),
    libraryDependencies ++= Seq(
      //"com.novocode" % "junit-interface" % "0.11" % "test",
      //("org.scala-lang.modules" %% "scala-parser-combinators" %  "1.1.2").withDottyCompat(scalaVersion.value),
      //("org.typelevel" %% "cats-core" % "2.1.1").withDottyCompat(scalaVersion.value),
      ///////
      ("be.doeraene" %%% "scalajs-jquery" % "1.0.0").withDottyCompat(scalaVersion.value), //"0.9.1",
      ///////
      ("org.scala-js" %%% "scalajs-dom" % "1.1.0").withDottyCompat(scalaVersion.value),//"0.9.1",
      ("com.lihaoyi" %%% "scalatags" % "0.9.1").withDottyCompat(scalaVersion.value) //"0.6.7",
    )
  )