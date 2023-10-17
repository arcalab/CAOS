package caos.frontend

import caos.frontend.Configurator.Example
import caos.frontend.widgets.Widget.Helper
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.*
import caos.sos
import caos.sos.*
import caos.view.OptionView.OptMermaid
import caos.view.*

import scala.language.implicitConversions

/**
 * A configurator instance, extending `Configurator`, describes all elements to
 * generate a web-frontend for CAOS to analyse a data structure of type `Stx`.
 *
 * The abstract methods and values include a `name`, a `parser:String=>Stx`,
 * a sequence of `examples`, and a sequence of `widgets`.
 * @tparam Stx Type of the data structure to be analysed.
 */
trait Configurator[Stx]:
  /** Name of the header of the web frontend. */
  val name: String
  /** Possible alternative name for the input widget. */
  def languageName: String = name // override to rename the input widget
  /** Parser to build the data structure under analysis */
  val parser: String=>Stx
  /** Sequence of examples */
  val examples: Iterable[Example] // name -> value
  /** Main widgets, on the right hand side of the screen */
  val widgets: Iterable[(String,WidgetInfo[Stx])]
  /** Secondary widgets, below the code */
  @deprecated(message = "Instead, for each WidgetInfo w, move it using `w.moveTo(1)`.")
  val smallWidgets: Iterable[(String,WidgetInfo[Stx])]=List()
  /** Documentation of the widgets. It can be presented as a list of triples `a->b->c`, representing
   *  (a) name of the widget being documented,
   *  (b) text when the mouse hovers over the "?", and
   *  (c) html text for the helper message.
   */
  val documentation: Documentation = Documentation()
  /** Footer message (HTML) */
  val footer: String = ""

/**
 * Provides functions that produce WidgetInfos, which describe widgets.
 */
object Configurator:

  /**
   * Generates a WidgetInfo that displays a view of the input program as text or a Mermaid diagram.
   * @param viewProg converts a program into a String
   * @param typ is the type of the String, typically "Text" or "Mermaid"
   * @tparam Stx is the type of the program (syntax)
   * @return the WidgetInfo describing how to create the widget
   */
  def view[Stx](viewProg:Stx=>String, typ:ViewType): WidgetInfo[Stx] =
    Visualize[Stx,Stx](x=>View(viewProg(x)),typ, x=>x)

  /**
   * Generates a WidgetInfo similar to `view`, but applied to all examples from the Example's widget.
   * @param viewProgs converts a sequence of programs into a String
   * @param typ is the type of the String, typically "Text" or "Mermaid"
   * @tparam Stx is the type of the program (syntax)
   * @return the WidgetInfo describing how to create the widget
   */
  def viewAll[Stx](viewProgs:Seq[(String,Stx)]=>String, typ:ViewType): WidgetInfo[Stx] =
    VisualizeAll[Stx,Stx](x=>View(viewProgs(x)),typ, x=>x)
  /**
   * Generates a Widget that can perform steps interactively for a given SOS and initial state.
   * @param initialSt is the initial state of the semantics
   * @param sos is the SOS object that captures how to evolve (semantics)
   * @param viewProg converts a program into a String
   * @param typ is the type of the String, typically "Text" or "Mermaid"
   * @tparam Stx is the type of the program (syntax)
   * @tparam A is the type of the actions (labels) of the semantics
   * @tparam S is the type of the states of the semantics
   * @return the WidgetInfo describing how to create the widget
   */
  def steps[Stx,A,S](initialSt:Stx=>S, sos:SOS[A,S], viewSt:S=>String,
                     viewAct:A=>String=((x:A)=>x.toString),typ:ViewType=Text): WidgetInfo[Stx] =
    Simulate[Stx,A,S](sos, x=>View(viewSt(x)), viewAct, typ, initialSt)

  /**
   * Generates Widget that can display multiple tabs with different views of the program.
   * @param viewProgs converts a program into a sequence of pairs of strings (title, content).
   * @param typ is the type of the String, typically `Text` or `Code("scala")`
   * @tparam Stx is the type of the program (syntax)
   * @return
   */
  def viewTabs[Stx](viewProgs:Stx=>List[(String,String)], typ:ViewType): WidgetInfo[Stx] =
    VisualizeTab[Stx,Stx](x=>viewProgs(x).map(y=>View(y._2)),typ, x=>viewProgs(x).map(_._1), x=>x)

  /**
   * Generates a Widget that produces multiple Mermaid diagrams representing different views of the program.
   * @param viewProgs converts a program into a sequence of pairs of string (title, mermaid-code).
   * @tparam Stx is the type of the program (syntax)
   * @return
   */
  def viewMerms[Stx](viewProgs:Stx=>List[(String,String)]): WidgetInfo[Stx] =
    VisualizeOpt[Stx,Stx](c => OptMermaid(viewProgs(c).toMap), Mermaid, x=>x)

  /**
   * Creates a widget that depicts an LTS with all reachable states, given an initial staten and an SOS.
   * @param initialSt is the initial state
   * @param sos is the SOS object that captures how to evolve (semantics)
   * @param viewSt converts a state into a String
   * @param viewAct convers an action into a String (default is `toString`)
   * @param maxSt is the maximum number of states (default is `80`)
   * @tparam Stx is the type of the program (syntax)
   * @tparam A is the type of actions
   * @tparam S is the type of states
   * @return the WidgetInfo describing how to create the LTS widget
   */
  def lts[Stx,A,S](initialSt:Stx=>S,sos:SOS[A,S],viewSt:S=>String,
                   viewAct:A=>String=((x:A)=>x.toString),maxSt:Int=80): WidgetInfo[Stx] =
    Visualize[Stx,Stx](x=>View(SOS.toMermaid(sos,initialSt(x),viewSt,viewAct,maxSt)), Mermaid, x=>x)


  /**
   * * Creates a widget that depicts an LTS, similar to `lts`, with all reachable states,
   * given an initial staten and an SOS. It builds step by step, expanding by clicking in a red
   * (unexplored) node, and starting with only the root expanded.
   * @param initialSt is the initial state
   * @param sos       is the SOS object that captures how to evolve (semantics)
   * @param viewSt    converts a state into a String
   * @param viewAct   convers an action into a String (default is `toString`)
   * @tparam Stx is the type of the program (syntax)
   * @tparam A   is the type of actions
   * @tparam S   is the type of states
   * @return the WidgetInfo describing how to create the LTS-Explore widget
   */
  def ltsExplore[Stx, A, S](initialSt: Stx => S, sos: SOS[A, S], viewSt: S => String,
                     viewAct: A => String = ((x: A) => x.toString)): WidgetInfo[Stx] =
      Explore[Stx,A,S](initialSt, sos, viewSt, viewAct)

  /**
   * Compare two elements using a comparison function
   * @param comp Binary comparison function that produces a String
   * @param t  is the type of the String, typically "Text" or "Mermaid"
   * @param pre1 is a function that produces the first element from the program
   * @param pre2 is a function that produces the second element from the program
   * @tparam Stx is the type of the program (syntax)
   * @tparam S1 is the type of the first element
   * @tparam S2 is the type of the second element
   * @return the WidgetInfo describing how to create the comparator widget
   */
  def compare[Stx,S1,S2](comp:(S1,S2)=>String, t:ViewType, pre1:Stx=>S1, pre2:Stx=>S2): WidgetInfo[Stx] =
    Visualize[Stx,String](View.apply,t,(c:Stx) => comp(pre1(c),pre2(c)))

  /**
   * Compare 2 SOSs using branching bisimulation, assuming that hidden actions implement "HasTaus".
   * @param sos1 is the SOS object that captures how to evolve the 1st term (semantics)
   * @param sos2 is the SOS object that captures how to evolve the 2nd term  (semantics)
   * @param pre1 is a function that produces the 1st term from the program
   * @param pre2 is a function that produces the 2nd term from the program
   * @param show1 is a function that converts a state of SOS1 to a string
   * @param show2 is a function that converts a state of SOS2 to a string
   * @param maxDepth (optional) is the maximum number of steps that it can make when searching for a bisimulation
   * @tparam Stx is the type of the program (syntax)
   * @tparam A  is the type of the actions for both SOS objects
   * @tparam S1 is the type of the states of SOS1
   * @tparam S2 is the type of the states of SOS2
   * @return the WidgetInfo describing how to create the comparator widget
   */
  def compareBranchBisim[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2,
                                      show1:S1=>String = (_:S1).toString, show2:S2=>String = (_:S2).toString,
                                      showAct:(A=>String) = (_:A).toString,
                                      maxDepth:Int=5000): WidgetInfo[Stx] =
    compare[Stx,S1,S2]((a,b)=>BranchBisim.findBisimPP(a,b,show1,show2,showAct)(using sos1,sos2,maxDepth),Text,pre1,pre2)

  /**
   * Compare 2 SOSs using strong bisimulation.
   * @param sos1 is the SOS object that captures how to evolve the 1st term (semantics)
   * @param sos2 is the SOS object that captures how to evolve the 2nd term  (semantics)
   * @param pre1 is a function that produces the 1st term from the program
   * @param pre2 is a function that produces the 2nd term from the program
   * @param show1 is a function that converts a state of SOS1 to a string
   * @param show2 is a function that converts a state of SOS2 to a string
   * @param maxDepth (optional) is the maximum number of steps that it can make when searching for a bisimulation
   * @tparam Stx is the type of the program (syntax)
   * @tparam A  is the type of the actions for both SOS objects
   * @tparam S1 is the type of the states of SOS1
   * @tparam S2 is the type of the states of SOS2
   * @return the WidgetInfo describing how to create the comparator widget
   */
  def compareStrongBisim[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2,
                                      show1:S1=>String = (_:S1).toString, show2:S2=>String = (_:S2).toString,
                                      showAct:(A=>String), // = (_:A).toString,
                                      maxDepth:Int=5000): WidgetInfo[Stx] =
    compare[Stx,S1,S2]((a,b)=>StrongBisim.findBisimPP(a,b,show1,show2,showAct)(using sos1,sos2,maxDepth),Text,pre1,pre2)

  /**
   * Compare 2 SOSs using trace equivalence.
   * @param sos1 is the SOS object that captures how to evolve (semantics)
   * @param sos2 is the SOS object that captures how to evolve (semantics)
   * @param pre1 is a function that produces the first element from the program
   * @param pre2 is a function that produces the second element from the program
   * @tparam Stx is the type of the program (syntax)
   * @tparam A  is the type of the actions for both SOS objects
   * @tparam S1 is the type of the first element
   * @tparam S2 is the type of the second element
   * @return the WidgetInfo describing how to create the comparator widget
   */
  def compareTraceEq[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2): WidgetInfo[Stx] =
    compare[Stx,S1,S2]((a,b)=>TraceEquiv(a,b,sos1,sos2),Text,pre1,pre2)

  /**
   * Run some analysis that only produces runtime errors (thrown) or a set of warnings,
   * and does so everytime the system is called.
   * @param a is a function that produces a sequence of warnings, possibly throwing errors
   * @tparam Stx is the type of the program (syntax)
   * @return the WidgetInfo describing how to create an `Analyser` widget
   */
  def check[Stx](a: Stx=>Seq[String]): WidgetInfo[Stx] =
    Analyse(a)

  /** Simple class to capture an example with a name and a description. */
  case class Example(example:String, name:String, description:String)

  /** Helper to build examples as `examples = List("name" -> "code")` */
  implicit def toExample(nameCode:(String,String)): Example =
    Example(nameCode._2,nameCode._1,"")
  /** Helper to build examples as `examples = List("name" -> "code" -> "description")` */
  implicit def toExampleDesc(nameCodeDesc:((String,String),String)): Example =
    Example(nameCodeDesc._1._2,nameCodeDesc._1._1,nameCodeDesc._2)
  implicit def toDocumentation(docs:List[((String,String),String)]): Documentation =
    Documentation().add(docs)


