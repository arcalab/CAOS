val scala3Version = "3.3.5"
val scalaParserCombinatorsVersion = "2.1.0"
val scalaJSDomVersion = "1.2.0"
val scalaJSQueryVersion = "1.0.0"
val scalaTagsVersion = "0.9.1"

lazy val caos = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "caos",
    version := "0.2",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-indent"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % scalaParserCombinatorsVersion,
      ("org.scala-js" %%% "scalajs-dom" % scalaJSDomVersion).cross(CrossVersion.for3Use2_13),
      ("be.doeraene" %%% "scalajs-jquery" % scalaJSQueryVersion).cross(CrossVersion.for3Use2_13),
      ("com.lihaoyi" %%% "scalatags" % scalaTagsVersion).cross(CrossVersion.for3Use2_13),
    ),
  )
