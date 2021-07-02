package caos.sos

//import choreo.syntax.Choreo.{Action, In, Out, Tau}


trait SOS[Act,State]:
  def next(s:State): Set[(Act,State)]
  def accepting(s:State): Boolean

trait HasTaus:
  val isTau: Boolean

//trait WSOS[Act<:HasTaus,State] extends SOS[Act,State]:
//  def nextWeak(s:State): Set[(Act,State,Option[State])] =
//    SOS.nextWeak(this,s,None)


object SOS:
  /////////////////////////
  // auxiliary functions //
  /////////////////////////

  /** All (a,s',Some(s_prev)) such that: s -tau-> ... -tau-> s_prev -a-> s',
   *  or all (a,s',None) such that s -tau-> s'
   */
  def nextWeak[A<:HasTaus,S](sos:SOS[A,S], s:S, last:Option[S]=None): Set[(A,S,Option[S])] =
    (for (a,s2)<-sos.next(s) yield
  a match
    case x if x.isTau => nextWeak(sos,s2,Some(s2))
    case x => Set((x,s2,last))
  ).flatten

  def byTau[A<:HasTaus,S](sos:SOS[A,S], s:S): Set[S] =
    (for (a,s2)<-sos.next(s) yield
  a match
    case t if t.isTau => byTau(sos,s2) + s
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

  /** Replace `next` by the weak version, possibly precedded by taus but NOT being a tau. */
  def postponeTaus[A<:HasTaus,S](sos:SOS[A,S]): SOS[A,S] =
    new SOS[A,S]:
      override def next(s: S): Set[(A, S)] =
        for (a2,s2,_) <- SOS.nextWeak(sos,s)
        yield (a2,s2)
      override def accepting(s: S): Boolean =
        sos.accepting(s)

          

