package caos.frontend

import caos.frontend.Configurator.Widget
import caos.sos
import caos.sos._
import caos.view.{Text, View}

trait Configurator[Stx]:
  val name: String
  type T = Stx
  val parser: String=>Stx
  def id(c:Stx):Stx = c
  val examples: Iterable[(String,String)] // name -> value
  /** Main widgets, on the right hand side of the screen */
  val widgets: Iterable[(String,Widget[Stx])]
  /** Secondary widgets, below the code */
  val smallWidgets: Iterable[(String,Widget[Stx])]=List()

object Configurator:
  sealed trait Widget[Stx]
  case class Visualize[Stx,S](v:S=>View,pre:Stx=>S)
    extends Widget[Stx]
  case class Simulate[Stx,A,S](sos:SOS[A,S],v:S=>View,pre:Stx=>S)
    extends Widget[Stx]
//  case class Compare[Stx,R,S1,S2](comp:(S1,S2)=>R, v:R=>View, pre1:Stx=>S1, pre2:Stx=>S2)
//    extends Widget[Stx]

  def compare[Stx,R,S1,S2](comp:(S1,S2)=>R, v:R=>View, pre1:Stx=>S1, pre2:Stx=>S2) =
    Visualize[Stx,R](v,(c:Stx) => comp(pre1(c),pre2(c)))

  // compare 2 SOSs using branching bisimulation
  def compareBranchBisim[Stx,A<:HasTaus,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2) =
    compare[Stx,String,S1,S2]((a,b)=>BranchBisim.findBisimPP(a,b)(using sos1,sos2),Text,pre1,pre2)
  def compareTraceEq[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2) =
    compare[Stx,String,S1,S2]((a,b)=>TraceEquiv(a,b,sos1,sos2),Text,pre1,pre2)

//  def project[Stx,S](p:Projection[_,S],v:View[Set[S],_],pre:Stx=>S): Visualize[Stx,Set[S]] =
//    Visualize(v, stx => p.allProj(pre(stx)))