package caos.frontend

import widgets.WidgetInfo
import WidgetInfo.*
import caos.sos
import caos.sos.*
import caos.common.Example
import caos.view.OptionView.OptMermaid
import caos.view.{Mermaid, OptionView, Text, View, ViewType}

trait Configurator[Stx]:
  val name: String
  def languageName: String = name // override to rename the input widget
  type T = Stx
  val parser: String=>Stx
  def id(c:Stx):Stx = c
  val examples: Iterable[Example] // name -> value
  /** Main widgets, on the right hand side of the screen */
  val widgets: Iterable[(String,WidgetInfo[Stx])]
  /** Secondary widgets, below the code */
  val smallWidgets: Iterable[(String,WidgetInfo[Stx])]=List()

/**
 * Provides functions that produce WidgetInfos, which describe widgets.
 */
object Configurator:

  /**
   * Generates a Visualize widget.
   * @param viewProg converts a program into a String
   * @param typ is the type of the String, typically "Text" or "Mermaid"
   * @tparam Stx is the type of the program (syntax)
   * @return the WidgetInfo describing how to create the widget
   */
  def view[Stx](viewProg:Stx=>String, typ:ViewType): WidgetInfo[Stx] =
    Visualize[Stx,Stx](x=>View(viewProg(x)),typ, x=>x)

  /**
   * Generates a Simulate widget
   * @param initialSt is the initial state of the semantics
   * @param sos is the SOS object that captures how to evolve (semantics)
   * @param viewProg converts a program into a String
   * @param typ is the type of the String, typically "Text" or "Mermaid"
   * @tparam Stx is the type of the program (syntax)
   * @tparam A is the type of the actions (labels) of the semantics
   * @tparam S is the type of the states of the semantics
   * @return the WidgetInfo describing how to create the widget
   */
  def steps[Stx,A,S](initialSt:Stx=>S, sos:SOS[A,S], viewProg:S=>String, typ:ViewType): WidgetInfo[Stx] =
    Simulate[Stx,A,S](sos, x=>View(viewProg(x)), typ, initialSt)

  /**
   * Generates a VisualizeTab widget
   * @param viewProgs converts a program into a String
   * @param typ is the type of the String, typically "Text" or "Mermaid"
   * @tparam Stx is the type of the program (syntax)
   * @return
   */
  def viewTabs[Stx](viewProgs:Stx=>List[(String,String)], typ:ViewType): WidgetInfo[Stx] =
    VisualizeTab[Stx,Stx](x=>viewProgs(x).map(y=>View(y._2)),typ, x=>viewProgs(x).map(_._1), x=>x)

  /**
   * Generates a VisualizeOpt widget
   * @param viewProgs
   * @tparam Stx is the type of the program (syntax)
   * @return
   */
  def viewMerms[Stx](viewProgs:Stx=>List[(String,String)]): WidgetInfo[Stx] =
    VisualizeOpt[Stx,Stx](c => OptMermaid(viewProgs(c).toMap), Mermaid, x=>x)

  /**
   * Creates a widget to vizualise a program
   * @param viewProg converts a program into a String
   * @param typ is the type of the String, typically "Text" or "Mermaid"
   * @tparam Stx is the type of the program (syntax)
   * @return the WidgetInfo describing how to create the widget
   */
  def viewWarn[Stx](viewProg:Stx=>String,typ: ViewType):WidgetInfo[Stx] =
    VisualizeWarning[Stx,Stx](x=>View(viewProg(x)),typ,x=>x)

  /**
   * Creates a widget that depicts an LTS with all reachable states, given an initial staten and an SOS
   * @param initialSt is the initial state
   * @param sos is the SOS object that captures how to evolve (semantics)
   * @param viewSt converts a state into a String
   * @param viewAct convers an action into a String
   * @param maxSt is the maximum number of states (default is 150)
   * @tparam Stx is the type of the program (syntax)
   * @tparam A is the type of actions
   * @tparam S is the type of states
   * @return the WidgetInfo describing how to create the LTS widget
   */
  def lts[Stx,A,S](initialSt:Stx=>S,sos:SOS[A,S],viewSt:S=>String,viewAct:A=>String,maxSt:Int=150): WidgetInfo[Stx] =
    Visualize[Stx,Stx](x=>View(SOS.toMermaid(sos,initialSt(x),viewSt,viewAct,maxSt)), Mermaid, x=>x)

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
  def compareBranchBisim[Stx,A<:HasTaus,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2): WidgetInfo[Stx] =
    compare[Stx,S1,S2]((a,b)=>BranchBisim.findBisimPP(a,b)(using sos1,sos2),Text,pre1,pre2)

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

//  def project[Stx,S](p:Projection[_,S],v:View[Set[S],_],pre:Stx=>S): Visualize[Stx,Set[S]] =
//    Visualize(v, stx => p.allProj(pre(stx)))

