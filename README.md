# CAOS

CAOS is a framework to support **c**omputer **a**ided design of 
structural **o**perational **s**emantics for formal models.

The framework is written in Scala 3 and can be imported as a submodule of any Scala project.
It can be used to produce interactive visualisations of formal models under development, 
by compiling into JavaScript (using Scala.js) to be interpreted by a browser.

Please, notice that this README and CAOS documentation are under construction. 
We recommend interested users to look at [Choreo](https://github.com/arcalab/choreo), 
which provides an example of how to import and use CAOS. 
A live website of the generated site by CAOS can be found at 
[http://arcalab.github.io/choreo/](http://arcalab.github.io/choreo/) 

# Requirements

* Scala version 2.13 or higher (to work with Scala 3)
* Scala building tools ([sbt](https://www.scala-sbt.org)) 
* Java Runtime Environment ([JRE](https://www.java.com/en/download/))  

# Using CAOS

CAOS is meant to be imported by an ongoing Scala project with a concrete Abstract Syntax Tree.

Alternatively, it is possible to start from a CAOS template, following the instructions described [here](https://github.com/arcalab/caos.g8) (this template uses an older version of CAOS).

Examples of projects that use CAOS include:

 - [Simple While-language](https://github.com/cister-labs/whilelang-scala) - [Snapshot](https://cister-labs.github.io/whilelang-scala/)
 - [Choreo: choreographies with strong choice and loops](https://github.com/arcalab/choreo) - [Snapshot](http://arcalab.github.io/choreo/)
 - [MPST APIs](https://github.com/arcalab/mpst-api) <!-- - [Snapshot](https://arcalab.github.io/mpst-api) -->

## Importing CAOS

We recommend to define CAOS as a git submodule of your own Scala project.
Under your project folder run the following command. 

```bash 
git submodule add https://github.com/arcalab/CAOS.git lib/caos
```

This will add CAOS as a new git submodule into a folder `lib/caos/`.

Assuming you have a project named `rootProject` with the following build.sbt:
```scala
lazy val rootProject = project.in(file("."))
  .settings(
    name := "rootProject",
    scalaVersion := <yourversion>, // 2.13 or higher
    ...,
    libraryDependencies ++= Seq(
        ...
    )
  )
```
you can import and set CAOS by adding the following definitions:

```scala
lazy val caos = project.in(file("lib/caos"))
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaVersion := "3.1.1")

lazy val rootProject = project.in(file("."))
    .settings(
        name := "rootProject",
        scalaVersion := <yourversion>, // 2.13 or higher
        ...,
        scalaJSUseMainModuleInitializer := true,
        // replace rootProject.Main by the correct path to your Main.scala class
        Compile / mainClass := Some("my.root.project.Main"),
        Compile / fastLinkJS / scalaJSLinkerOutputDirectory := 
            baseDirectory.value / "lib" / "caos"/ "tool" / "js" / "gen",
        libraryDependencies ++= Seq(
            ...,
        )
  ).dependsOn(caos)
```

Note that the scalaVersion of your project should typically match the one of CAOS.

You will also need to add the plugin for ScalaJS by appending to the file `project/plugins.sbt` (create file if it does not exist yet):

```scala
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.7.1")
```

# Instantiating CAOS

CAOS provides an interface (formally a `trait` in Scala) called `Configurator`:

```scala
// CAOS Configurator 

trait Configurator[Stx]:
  val name: String
  val parser: String=>Stx
  val examples: Iterable[(String,Stx)]
  /** Main widgets, on the right hand side of the screen */
  val widgets: Iterable[(String,Widget[Stx])]
  /** Secondary widgets, below the code */
  val smallWidgets: Iterable[(String,Widget[Stx])]=Nil

object Configurator:

  // Visualisers
  def view[Stx](viewProg:Stx=>String, typ:ViewType): WidgetInfo[Stx]
  def viewTabs[Stx](viewProgs:Stx=>List[(String,String)], typ:ViewType): WidgetInfo[Stx] =
  def viewMerms[Stx](viewProgs:Stx=>List[(String,String)]): WidgetInfo[Stx] =
  def viewWarn[Stx](viewProg:Stx=>String,typ: ViewType):WidgetInfo[Stx] =

  // Animators
  def steps[Stx,A,S](initialSt:Stx=>S, sos:SOS[A,S], viewProg:S=>String, typ:ViewType): WidgetInfo[Stx] =
  def lts[Stx,A,S](initialSt:Stx=>S,sos:SOS[A,S],viewSt:S=>String,viewAct:A=>String,maxSt:Int=80): WidgetInfo[Stx] =

  // Comparing semantics
  def compare[Stx,S1,S2](comp:(S1,S2)=>String, t:ViewType, pre1:Stx=>S1, pre2:Stx=>S2): WidgetInfo[Stx] =
  def compareBranchBisim[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2): WidgetInfo[Stx] =
  def compareTraceEq[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2): WidgetInfo[Stx] =
```
To use CAOS you need to instantiate the `Configurator[A]` trait, 
for example in a class:
```scala 
class ConcreteConfigurator extends Configurator[ConcreteStx]: 
    ...
```
A full example can be found, e.g., in the [configurator in Choreo's project](https://github.com/arcalab/choreo/blob/196f751eafb07f26910220085759cc458bd01f07/src/main/scala/choreo/frontend/ChoreoSOSme.scala).

Finally, you need to call from your `Main` class to the `initSite` mehtod in CAOS. 

```scala 
// Root Project Main class 
package my.root.project
import caos.frontend.Site.initSite

object Main {
  def main(args: Array[String]):Unit = {
    initSite[ConcretStx](ConcreteConfigurator)
  }
}
```

# Compiling CAOS

In root project you need to run `sbt` to compile using ScalaJS's compiler:

```bash
sbt fastLinkJS
```

The resulting web page, already linked to the compiled JavaScript, can be found in 
`lib/tool/index.html`, if you imported CAOS, or in the select `<tool_path>/index.html`, 
if you used the CAOS template.
