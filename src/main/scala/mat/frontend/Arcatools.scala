package mat.frontend

//import mat.analysis.Bisimulation
//import mat.analysis.other.SyntAnalysis
import mat.frontend.Arcatools.Widget
//import mat.pomsets.{Choreo2Pom, PomDefSOS, Pomset}
//import mat.projection.{ChorDefProj, Projection}
import mat.sos._
//import mat.syntax.Choreo.Action
//import mat.syntax.{Agent, Choreo}
import mat.view.View
import mat.{sos}

trait Arcatools[Stx]:
  val name: String
  type T = Stx
  val parser: String=>Stx
  val examples: Iterable[(String,Stx)]
  val widgets: Iterable[(Widget[Stx],String)]
  val smallWidgets: Iterable[(Widget[Stx],String)]

object Arcatools:
//  type GetView[A] = A => View
//  type Transform[From,To] = From=>To
  sealed trait Widget[Stx]
  case class Visualize[Stx,S](v:S=>View,pre:Stx=>S)
    extends Widget[Stx]
  case class Simulate[Stx,A,S](sos:SOS[A,S],v:S=>View,pre:Stx=>S)
    extends Widget[Stx]
  case class Compare[Stx,R,S1,S2](comp:(S1,S2)=>R, v:R=>View, pre1:Stx=>S1, pre2:Stx=>S2)
    extends Widget[Stx]


  // constructors for no pre-processing (id)
  def visualize[Stx](v:Stx=>View): Visualize[Stx,Stx] = Visualize(v,c=>c)
  def simulate[Stx,A](sos:SOS[A,Stx],v:Stx=>View): Simulate[Stx,A,Stx] = Simulate(sos,v,c=>c)

  // compare 2 SOSs using branching bisimulation
  def compareBranchBisim[Stx,A<:HasTaus,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2) =
    Compare[Stx,String,S1,S2]((a,b)=>BranchBisim.findBisimPP(a,b)(using sos1,sos2),mat.view.Text,pre1,pre2)
  def compareTraceEq[Stx,A,S1,S2](sos1:SOS[A,S1],sos2:SOS[A,S2],pre1:Stx=>S1,pre2:Stx=>S2) =
    Compare[Stx,String,S1,S2]((a,b)=>TraceEquiv(a,b,sos1,sos2),mat.view.Text,pre1,pre2)


//  def project[Stx,S](p:Projection[_,S],v:View[Set[S],_],pre:Stx=>S): Visualize[Stx,Set[S]] =
//    Visualize(v, stx => p.allProj(pre(stx)))