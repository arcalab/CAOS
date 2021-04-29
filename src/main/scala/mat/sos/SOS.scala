package mat.sos

//import choreo.syntax.Choreo.{Action, In, Out, Tau}


trait SOS[Act,State]:
  def next(s:State): Set[(Act,State)]
  def accepting(s:State): Boolean

trait HasTaus:
  val isTau: Boolean

trait WSOS[Act<:HasTaus,State] extends SOS[Act,State]:
  def nextWeak(s:State): Set[(Act,State,Option[State])] =
    SOS.nextWeak(this,s,None)


object SOS:
  /////////////////////////
  // auxiliary functions //
  /////////////////////////

  /** All (a,s'') such that: s -tau->* s' -a-> s''  */
  def nextWeak[A<:HasTaus,S](sos:WSOS[A,S], s:S, last:Option[S]=None): Set[(A,S,Option[S])] =
    (for (a,s2)<-sos.next(s) yield
  a match
    case x:HasTaus if x.isTau => nextWeak(sos,s2,Some(s2))
    case x => Set((x,s2,last))
  ).flatten

  def byTau[S](sos:WSOS[_,S], s:S): Set[S] =
    (for (a,s2)<-sos.next(s) yield
  a match
    case t:HasTaus if t.isTau => byTau(sos,s2) + s
    case x => Set(s))
  .flatten + s

  //  def transBy(a:Action): LTS[S]
  def nextPP[A,S](lts:SOS[A,S], s:S): String = lts.next(s)
    .map(p=>s"${p._1} ~~> ${p._2}")
    .mkString("\n")

  def nexts[A,S](sos:SOS[A,S], s:S, n:Int): Set[(List[A],Option[S])] = n match
    case 0 => Set(Nil -> Some(s))
    case _ =>
      val nc = sos.next(s)
      nc.flatMap(p=> {
        val rec = nexts(sos,p._2,n-1)
        if rec.isEmpty then
          List(List(p._1) -> None)
        else
          for s <- rec
            yield (p._1::s._1) -> s._2
      })

  def reachable[A,S](sos:SOS[A,S], s:S): Set[S] =
    var done:Set[S] = Set(s)
    var next = (for (a,s2) <- sos.next(s) yield s2) + s
    while next.nonEmpty do
      val x = next.head
      next -= x
      val next2 = (for (_,x2) <- sos.next(x) yield x2) -- done
      done ++= next2
      next ++= next2
    done
          

