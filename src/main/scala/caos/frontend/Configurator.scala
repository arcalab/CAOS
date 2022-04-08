package caos.frontend

import caos.frontend.Configurator.Widget
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
  val widgets: Iterable[(String,Widget[Stx])]
  /** Secondary widgets, below the code */
  val smallWidgets: Iterable[(String,Widget[Stx])]=List()

object Configurator:
  sealed trait Widget[Stx]
  case class Visualize[Stx,S](v:S=>View, typ:ViewType, pre:Stx=>S)
    extends Widget[Stx]
  case class Simulate[Stx,A,S](sos:SOS[A,S],v:S=>View,typ:ViewType,pre:Stx=>S)
    extends Widget[Stx]
  case class VisualizeTab[Stx,S](v:S=>List[View],typ:ViewType,t:S=>List[String],pre:Stx=>S)
    extends Widget[Stx]
  case class VisualizeWarning[Stx,S](v:S=>View, typ:ViewType, pre:Stx=>S)
    extends Widget[Stx]

  // shorthands/helpers
  /** Generates a Visualize widget */
  def view[Stx](calc:Stx=>String, typ:ViewType): Widget[Stx] =
    Visualize[Stx,Stx](x=>View(calc(x)),typ, x=>x)
  /** Generates a Simulate widget */
  def steps[Stx,A,S](prepare:Stx=>S,sos:SOS[A,S], calc:S=>String, typ:ViewType): Widget[Stx] =
    Simulate[Stx,A,S](sos, x=>View(calc(x)), typ, prepare)
  /** Generates a VisualizeTab widget */
  def viewTabs[Stx](calc:Stx=>List[(String,String)], typ:ViewType): Widget[Stx] =
    VisualizeTab[Stx,Stx](x=>calc(x).map(y=>View(y._2)),typ, x=>calc(x).map(_._1), x=>x)
  /** Generates a VisualizeOpt widget */
  def viewMerms[Stx](calc:Stx=>List[(String,String)]): Widget[Stx] =
    VisualizeOpt[Stx,Stx](c => OptMermaid(calc(c).toMap), Mermaid, x=>x)

  def viewWarn[Stx](calc:Stx=>String,typ: ViewType):Widget[Stx] =
    VisualizeWarning[Stx,Stx](x=>View(calc(x)),typ,x=>x)

  def compare[Stx,R,S1,S2](comp:(S1,S2)=>R, v:R=>View, t:ViewType, pre1:Stx=>S1, pre2:Stx=>S2): Widget[Stx] =
    Visualize[Stx,R](v,t,(c:Stx) => comp(pre1(c),pre2(c)))

  // compare 2 SOSs using branching bisimulation
  def compareBranchBisim[Stx,A<:HasTaus,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2): Widget[Stx] =
    compare[Stx,String,S1,S2]((a,b)=>BranchBisim.findBisimPP(a,b)(using sos1,sos2),View.apply,Text,pre1,pre2)
  def compareTraceEq[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2): Widget[Stx] =
    compare[Stx,String,S1,S2]((a,b)=>TraceEquiv(a,b,sos1,sos2),View.apply,Text,pre1,pre2)

//  def project[Stx,S](p:Projection[_,S],v:View[Set[S],_],pre:Stx=>S): Visualize[Stx,Set[S]] =
//    Visualize(v, stx => p.allProj(pre(stx)))

  // experiment
  case class VisualizeOpt[Stx,S](v:S=>OptionView,t:ViewType,pre:Stx=>S)
    extends Widget[Stx]