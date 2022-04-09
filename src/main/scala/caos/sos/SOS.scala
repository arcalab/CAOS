package caos.sos

//import choreo.syntax.Choreo.{Action, In, Out, Tau}


trait SOS[Act,State]:
  def next(s:State): Set[(Act,State)]
  def accepting(s:State): Boolean

trait HasTaus:
  val isTau: Boolean

//// experiment: SOS with ints
//object ISOS extends SOS[Int,Int]:
//  def next(s:Int): Set[(Int,Int)] =
//    if s>6 then Set()
//    else (for i <- 1 to 3 yield (i,s+i)).toSet
//  def accepting(s:Int): Boolean = s>6

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
  def nextWeak[A,S](sos:SOS[A,S], s:S, last:Option[S]=None): Set[(A,S,Option[S])] =
    (for (a,s2)<-sos.next(s) yield
      a match
        case x:HasTaus if x.isTau => nextWeak(sos,s2,Some(s2))
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

  def toMermaid[A,S](sos:SOS[A,S],s:S,showSt:S=>String,
                     showAct:A=>String, max:Int = 150): String =
    var i = 0
    var _ids: Map[S,Int] = Map()
    def ids(s:S): Int =
      if _ids.contains(s) then
        _ids(s)
      else
        _ids+=s->i
        i+=1
        i-1
    def fix(s:String): String = s"\"$s\""
      .replaceAll("\n","<br>")
//      .replaceAll("\\[|\\]|\\(|\\)","_")
//      .replaceAll(";",",")
//      .replaceAll("\\|","#")
    def aux(next:Set[S],done:Set[S],limit:Int): String =
      if limit <=0 then
        return (for n<-next yield s"\n  style ${ids(n)} fill:#f87,stroke:#633,stroke-width:4px;").mkString
      next.headOption match
        case Some(st) if done contains st => aux(next-st,done,limit)
        case Some(st) =>
          var done2 = done+st
          var next2 = next-st
          var res = s"\n  ${ids(st)}[${fix(showSt(st))}];"
          for (a,s2) <- sos.next(st) do
            next2 += s2
            res += s"\n  ${ids(s2)}[${fix(showSt(s2))}];\n  ${ids(st)} -->|${fix(showAct(a))}| ${ids(s2)};"
          res + aux(next2,done2,limit-1)
        case None => ""
    "graph TD\n  style 0 fill:#8f7,stroke:#363,stroke-width:4px;" + aux(Set(s),Set(),max)


          

