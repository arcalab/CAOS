package caos.sos

import scala.scalajs.js.annotation.JSExportTopLevel

/** An SOS semantics with states in `State` and labels in `Act` needs to implement
 * a function that provides the follow-up states */
trait SOS[+Act,State]:
  /** Set of next states `State` and corresponding labels `Act`. */
  def next[A>:Act](s:State): Set[(A,State)]
  /** Check if a given state is accepting. */
  def accepting(s:State): Boolean = false

/** For weak semantics, a transition is invisible/internal if
 *  it extends `HasTaus` and `isTau` holds. */
trait HasTaus:
  val isTau: Boolean


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

  /** Given a state `s` calculates all targets `t` such that `s --tau*--> t` */
  def byTau[A/*<:HasTaus*/,S](sos:SOS[A,S], s:S): Set[S] =
    (for (a,s2)<-sos.next(s) yield
      a match
        case t:HasTaus if t.isTau => byTau(sos,s2) + s
        case _ => Set(s))
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
  def postponeTaus[A/*<:HasTaus*/,S](sos:SOS[A,S]): SOS[A,S] =
    new SOS[A,S]:
      override def next[A2>:A](s: S): Set[(A2, S)] =
        for (a2,s2,_) <- SOS.nextWeak(sos,s)
        yield (a2,s2)
      override def accepting(s: S): Boolean =
        sos.accepting(s)

  def toMermaid[A,S](sos:SOS[A,S], s:S, showSt:S=>String,
                     showAct:A=>String, maxNodes:Int): String =
    var i = 0
    var _ids: Map[S,Int] = Map()
    def ids(s:S): Int =
      if _ids.contains(s) then
        _ids(s)
      else
        _ids+=s->i
        i+=1
        i-1
    def fix(s:String): String = if s.startsWith("$$") then s.drop(2) else
      s"\" $s\""
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
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
          val done2 = done+st
          var next2 = next-st
          var res = s"\n  ${ids(st)}([${fix(showSt(st))}]);"
          for (a,s2) <- sos.next(st) do
            next2 += s2
            res += s"\n  ${ids(s2)}([${fix(showSt(s2))}]);\n  ${ids(st)} -->|${fix(showAct(a))}| ${ids(s2)};"
          res + aux(next2,done2,limit-1)
        case None => ""
    "graph TD\n  style 0 fill:#8f7,stroke:#363,stroke-width:4px;" + aux(Set(s),Set(),maxNodes)

  def traverse[A,S](sos:SOS[A,S], s:S, max:Int=5000): (Set[S],Int,Boolean) =
    def aux(next:Set[S],done:Set[S],edges:Int, limit:Int): (Set[S],Int,Boolean) =
      if limit <=0 then
        return (done,edges,false)
      next.headOption match
        case None =>
          (done, edges, true)
        case Some(st) if done contains st =>
          aux(next-st,done,edges,limit)
        case Some(st) => //visiting new state
          val more = sos.next(st)
          aux((next-st)++more.map(_._2), done+st, edges+more.size,limit-1)

    aux(Set(s), Set(), 0, max)



          

