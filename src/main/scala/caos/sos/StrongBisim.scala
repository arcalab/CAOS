package caos.sos

import caos.common.Multiset._
import caos.sos.SOS._
import caos.sos._

/** Strong bisimulation.
 * Built based on simplifications of BranchBisim.
 * In the future it could extend BranchBisim. */
object StrongBisim extends Bisimulation:
  ///////////////////////////
  /// Strong bisimulation ///
  ///////////////////////////

  /** Find a strong bisimulation. */
  def findBisim[A,G,L](g:G,l:L)(using gs:SOS[A,G], ls:SOS[A,L],stopAt:Int=5000): BResult[A,G,L] =
    findWBisim2Aux(Map(),Map((g,l)->Nil),Set(),Nil,1)

  private def findWBisim2Aux[A,G,L](visited:RT[A,G,L],
                                  missing:RT[A,G,L],
                                  triedHash:Set[Int], // to avoid redundant searches
                                  //                                          lastError:List[String],
                                  history:List[Int],
                                  i:Int) // to count how many runs
                                 (using gs:SOS[A,G],ls:SOS[A,L],stopAt:Int)
  : BResult[A,G,L] =
    // println(s"[Sim] $visited  --  $missing")
    if i >= stopAt /*5000/800000*/ then
      return Left(BEvid(Set(List("timeout",s"visited: $visited",s"missing: $missing")),triedHash,i))
    missing.headOption match
      // Success!
      case None => Right(visited)

      // Already visited
      case Some(ab,t) if visited contains ab =>
        findWBisim2Aux(visited,missing-ab,triedHash,history,i)

      // Fail: not equally accepting
      case Some(((g:G,l:L),t)) if gs.accepting(g) != ls.accepting(l) =>
        if gs.accepting(g) then
          Left(BEvid(Set(List(s"after ${if t.isEmpty then "[]" else t.reverse.mkString(",")}",s"$g is accepting",s"$l is not")),triedHash,i)) else
          Left(BEvid(Set(List(s"after ${if t.isEmpty then "[]" else t.reverse.mkString(",")}",s"$l is accepting",s"$g is not")),triedHash,i))

      // traverse steps...
      case Some(((g:G,l:L),t)) =>
        //        if i % 500 == 0 then
        //        println(s"\n#####\n[Sim] Round $i - ${history.reverse.mkString(".")} @ $g")// -- doing ${(g.trans.map(_._1)++l.trans.map(_._1)).toSet.mkString(",")}")
        //        println(s"[Sim] Round $i - ${history.reverse.mkString(".")} @ $g")// -- doing ${(g.trans.map(_._1)++l.trans.map(_._1)).toSet.mkString(",")}")

        // for every cs1 --a1-> cs1',
        //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]


        // Collect all candidates to add to the bisimulation
        val moreGL = collectMore(g,l,t) match
          case Left(err) => return Left(BEvid(Set(err),triedHash,i))
          case Right(m) => m
        val moreLG = collectMore(l,g,t) match
          case Left(err) => return Left(BEvid(Set(err),triedHash,i))
          case Right(m) => swap(m)
        var more:S[A,G,L] = and( moreGL, moreLG)


        //// Collected all candidates to add to the bisimulation (`more`)
        //// Now we need to prune repeated steps, and try all options (collecting info when one branch fails).

        /// Avoiding recurrent paths...
        val newTry = (missing.keys,more.map(_.keys)).hashCode
        //        val newTry = (visited.keys,missing,more.map(_.keys)).hashCode
        if triedHash contains newTry then
        //          println(s"[Sim] Tried $newTry -> FAIL")
          return Left(BEvid(Set(),triedHash,i))
        // Left(BEvid(lastError,triedHash,i))
        //findWBisim2Aux(visited,missing-((g,l)),triedHash,i+1)


        // check if, for any m<-more, a bisimulation can be found with `visited + m`
        var failed: Option[BEvid] = None
        var newTries = triedHash+newTry
        //        var newError = lastError
        var round = i

        //        if more.size>500 then
        //          println(s"[Sim] ($i - ${visited.hashCode} - ${more.hashCode}) options to visit: ${more.size}") //  \n"+more.map(_.hashCode).mkString("\n-----\n"))
        //          println(s"[Sim] ($i - $newTry) options to visit: ${more.size}\n"+more.map(
        //            m => m.toList.map(x=>s" - ${x._2} -> ${x._1._1} <-> ${x._1._2} )").mkString("\n")).mkString("\n-----\n"))
        var go=1

        while (more.nonEmpty) do
          val m = more.head // next option to try
          //          println(s"[Sim] ${history.reverse.mkString(".")}.$go NEXT: $newTry/$newTries/${m.keys.mkString(", ")}/${missing.keys.mkString(", ")}")
          findWBisim2Aux(visited + ((g,l)->t), missing++m, newTries, go::history, round+1) match
            case Right(value) =>
              //              println(s"[Sim] YES: $go/$newTry/${m.keys.mkString(", ")}")
              return Right(value)
            case Left(err) =>
              err.msgs.headOption match
                case Some("timeout"::_) => return Left(err)
                case _ =>

              //              println(s"[Sim] nope: $go/$newTry/${m.keys.mkString(", ")}")
              newTries ++= err.tried
              round = err.count
              go += 1
              failed = failed match
                case None => Some(err)
                case Some(e@BEvid(msgs,tried,count)) =>
                  Some(BEvid(msgs++err.msgs,newTries,round))
              //              newError = err.msg
              more -= m

        failed match
          case Some(err) => Left(err)
          case None => Right(visited)



  private def collectMore[A,G,L](g:G, l:L, t:List[A])
                                         (using gs:SOS[A,G], ls:SOS[A,L]): Either[List[String], S[A,G,L]] =
    var more:S[A,G,L] = none
    // for every g--a->g2
    for (a,g2)<- gs.next(g) do
    //      println(s"\n### doing $a\n[Sim] G $g ")
        // find matching l--a1->* s2
        val tr = ls.next(l) //SOS.nextWeak(ls,l)
        ///// more = set([])
        val mbMatch = for (a2,l2)<-tr if a==a2
          yield
            // found l--a->l2 to match g--a->g2
            one(g2,l2,a::t)
        if mbMatch.isEmpty then
        //              println(s"[Sim] L $l FAILS")
          return Left(List(s"after ${if t.isEmpty then "[]" else t.reverse.mkString(",")}",
            s"$g can do $a",
            s"$l cannot do τ*,$a"))
        //        println(s"[Sim] L $l matches") // by:\n - ${tr.filter(_._1==a).map(x=>s"${x._2} [${x._3}]").mkString("\n - ")}")
        //        println(s"[Sim] Adding to the bisim:\n + $g2\n - ${tr.filter(_._1==a).map(x=>s"${x._2} {tau: ${x._3}}").mkString("\n - ")}")
        more = and(more , ors(mbMatch)) //mbMatch.flatten
      //        println(s"[Sim] (Global/Local 'more'): ${more.size}\n"+more.map(
      //            m => m.toList.map(x=>s" - ${x._2} -> ${x._1._1} <-> ${x._1._2} )").mkString("\n")).mkString("\n-----\n"))
      //            println(s"[B] new mbMatch: $mbMatch")
    Right(more)




  // utils
  private def one[A,G,L](g:G, l:L, t:List[A]):S[A,G,L] = Set(Map((g,l)->t))
  private def none[A,G,L]:S[A,G,L] = Set(Map())
  private def add[A,G,L](m:S[A,G,L], g:G, l:L, t:List[A]):S[A,G,L] = m.map(_+((g,l)->t))
  private def and[A,G,L](x:S[A,G,L], y:S[A,G,L]): S[A,G,L] =
    for m1<-x; m2<-y yield m1++m2
  private def or[A,G,L](x:S[A,G,L], y:S[A,G,L]): S[A,G,L] =
    x ++ y
  private def ors[A,G,L](x:Set[S[A,G,L]]): S[A,G,L] =
    x.flatten
  private def swap[A,G,L](x:S[A,G,L]):S[A,L,G] =
    x.map(_.map(tpl => ((tpl._1._2,tpl._1._1),tpl._2)))

