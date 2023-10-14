# CAOS

CAOS is a framework to support **c**omputer **a**ided design of 
structural **o**perational **s**emantics for formal models.

The framework is written in Scala 3 and can be imported as a submodule of any Scala project.
It can be used to produce interactive visualisations of formal models under development, 
by compiling into JavaScript (using Scala.js) to be interpreted by a browser.

<!-- Please, notice that this README and CAOS documentation are under construction. 
We recommend interested users to look at [Choreo](https://github.com/arcalab/choreo), 
which provides an example of how to import and use CAOS. 
A live website of the generated site by CAOS can be found at 
[http://arcalab.github.io/choreo/](http://arcalab.github.io/choreo/) 
 -->
# Requirements

* Scala version 2.13 or higher (to work with Scala 3)
* Scala building tools ([sbt](https://www.scala-sbt.org)) 
* Java Runtime Environment ([JRE](https://www.java.com/en/download/))  

# Using CAOS

CAOS is meant to be imported by an ongoing Scala project with a concrete Abstract Syntax Tree. The best way to learn how to use it is to follow examples of projects that use CAOS.

Examples of such projects include:

 - [Simple While-language](https://cister-labs.github.io/whilelang-scala/) [(source)](https://github.com/cister-labs/whilelang-scala)
 - [Simple Lambda Calculus](http://arcalab.github.io/lambda-caos) [(source)](https://github.com/arcalab/lambda-caos)
 - [Branching Pomset Encoder](http://lmf.di.uminho.pt/b-pomset/) [(ICE'22 paper+source)](https://jose.proenca.org/publication/edixhoven-branching-2022/)
 - [Pompset: Scala API generation of MPST via Pomsets](http://lmf.di.uminho.pt/pompset) [(ECOOP'22 paper+source)](https://jose.proenca.org/publication/cledou-apigeneration-2022/)
 - [ST4MP: Session Types for Multilanguage Programming](http://lmf.di.uminho.pt/st4mp/) [(ISoLA'22 paper+source)](https://jose.proenca.org/publication/jongmans-st4mp-2022/)
 - [Ceta: Choreographic Extended Team Automata – realisability experiments](http://lmf.di.uminho.pt/ceta)
 - [Choreo: experiments with choreographies with strong choice and loops](http://arcalab.github.io/choreo/) [(source)](https://github.com/arcalab/choreo)
 - [Mars: early experiments on a language to generate runtime monitors](https://mars-language.bitbucket.io) [(RTSS'20 short paper)](https://jose.proenca.org/publication/nandi-dsl-2020/)
 - [Marx: experimental reactive language for synchronous architectures](http://lmf.di.uminho.pt/marx/) [(source)](https://github.com/arcalab/marx)
 - [VVML: animator and analyser of a subset of UML Activity Diagrams](https://cister-labs.github.io/coreVVML/) [(source)](https://github.com/cister-labs/coreVVML/)
 - [Apoo: animator of a simple assembly language](https://cister-labs.github.io/apoo)[(Apoo's main page)](https://www.dcc.fc.up.pt/~rvr/naulas/apoo/)
 - [CCS: animator of Milner's Calculus of Communicating Systems](http://lmf.di.uminho.pt/ccs-caos/)

Alternatively, it is possible to start from a CAOS template, following the instructions described [here](https://github.com/arcalab/caos.g8) (this template uses an older version of CAOS).

## Tutorial on CAOS

The rest of this document describes the essentials needed to use CAOS. A more detailed tutorial can be found online:

 - Caos' tutorial: https://arxiv.org/abs/2304.14901
 - Caos' demo video: https://youtu.be/Xcfn3zqpubw 


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
  ///// Interface of a configurator for CAOS /////
  /** Name of the project */
  val name: String
  /** Optional: name of the input language */
  def languageName: String
  /** How to create an AST from text */
  val parser: String=>Stx
  /** List of examples *//
  val examples: Iterable[(String,Stx)]
  /** Main widgets, on the right hand side of the screen */
  val widgets: Iterable[(String,WidgetInfo[Stx])]
  /** Secondary widgets, below the code */
  val smallWidgets: Iterable[(String,WidgetInfo[Stx])]=Nil

object Configurator:
  ///// Constructors for widgets //////
  // Visualisers:
  def view[Stx](viewProg:Stx=>String, typ:ViewType): WidgetInfo[Stx]
  def viewTabs[Stx](viewProgs:Stx=>List[(String,String)], typ:ViewType): WidgetInfo[Stx]
  def viewMerms[Stx](viewProgs:Stx=>List[(String,String)]): WidgetInfo[Stx]
  def viewWarn[Stx](viewProg:Stx=>String,typ: ViewType):WidgetInfo[Stx]

  // Animators:
  def steps[Stx,A,S](initialSt:Stx=>S, sos:SOS[A,S], viewProg:S=>String, typ:ViewType): WidgetInfo[Stx]
  def lts[Stx,A,S](initialSt:Stx=>S,sos:SOS[A,S],viewSt:S=>String,viewAct:A=>String,maxSt:Int=80): WidgetInfo[Stx]

  // Comparing semantics:
  def compare[Stx,S1,S2](comp:(S1,S2)=>String, t:ViewType, pre1:Stx=>S1, pre2:Stx=>S2): WidgetInfo[Stx]
  def compareBranchBisim[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2): WidgetInfo[Stx]
  def compareTraceEq[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2): WidgetInfo[Stx]
  
  // Mandatory checks (throw errors and return warnings):
  def check[Stx](a: Stx=>Seq[String]): WidgetInfo[Stx]
```
To use CAOS you need to instantiate the `Configurator[A]` trait, 
for example in a classe. For example, for a `MyLanguage` object you could define:
```scala 
class MyConcreteConfigurator extends Configurator[MyLanguage]: 
    //...
    val widgets = List(
      "View parsed data" -> view(x=>x.toString , Text),
      "View pretty diagram" -> view(MyGraph.buildMermaid, Mermaid),
      "Run small-steps" -> lts(
        myBuildInitState, mySmallSOS, myPrintState, myPrintLabels),
      ...
    )
```

Currently we can visualise plain text and [Mermaid](https://mermaid-js.github.io) diagrams.
A full example can be found, e.g., in the [configurator in Choreo's project](https://github.com/arcalab/choreo/blob/8e5cb787595da87266956741bc77c72dac7eab9a/src/main/scala/choreo/frontend/ChoreoSOSme.scala).



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
