package caos.sos

import caos.common.Multiset._
import caos.sos.SOS._
import caos.sos._


object BranchBisim :
  //////////////////////////////
  /// Branching bisimulation ///
  //////////////////////////////

  private type R[A,B] = Set[(A,B)]
  private type RT[Act,A,B] = Map[(A,B),List[Act]] // relation with traces used to get there.
  private type S[Act,A,B] = Set[RT[Act,A,B]]
  //  type RC = R[Choreo,LocalBasic]
  //  type RC2 = R[GlobalTau,LocalManyTaus]

  /** Evidences that something that went wrong when searching for a bisimulation. */
  case class BEvid(msgs:Set[List[String]],tried:Set[Int],count:Int)
  /** Result from a bisimulation search: either some evidence of what went wrong, or a bisimulation */
  type BResult[Act,A,B] = Either[BEvid,RT[Act,A,B]]

  /** Pretty printing bisimulation results. */
  def pp[A,G,L](res: BResult[A,G,L]): String = res match
    case Left(err:BEvid) => "Not a bisim."+err.msgs.map(m => m.map("\n - "+_).mkString).mkString("\n---")
    case Right(rel) => pp2[A,G,L](rel) //println(rel.map(p=>s"- ${p._1}   <->   ${p._2}").mkString("\n"))

  /** Pretty printing bisimulations. */
  def pp2[A,G,L](rel:RT[A,G,L]): String =
    val strs = rel.toList.map((xy,t)=>
      (t.size,xy._1.toString,xy._2.toString,
        if t.isEmpty then "init" else t.mkString(",")))
      .toList.sorted
    val max = strs.map(_._2.size).max
    val strs2 = strs.map((_,x,y,t)=>(x+(" "*(max-x.size)),y,t))
    strs2.map(p=>s"- ${p._1}  <->  ${p._2}  @ ${p._3}").mkString("\n")

  //// Actual implementtion of branching bisimulation search ////

  /** Find a branching bisimulation and returns an explanation */
  def findBisimPP[A<:HasTaus,G,L](g:G,l:L)(using gs:SOS[A,G], ls:SOS[A,L],stopAt:Int=5000): String =
    pp(findWBisim2Aux(Map(),Map((g,l)->Nil),Set(),Nil,1))

  /** Find a branching bisimulation. */
  def findBisim[A<:HasTaus,G,L](g:G,l:L)(using gs:SOS[A,G], ls:SOS[A,L],stopAt:Int=5000): BResult[A,G,L] =
    findWBisim2Aux(Map(),Map((g,l)->Nil),Set(),Nil,1)

  private def findWBisim2Aux[A<:HasTaus,G,L](visited:RT[A,G,L],
                                  missing:RT[A,G,L],
                                  triedHash:Set[Int], // to avoid redundant searches
                                  //                                          lastError:List[String],
                                  history:List[Int],
                                  i:Int) // to count how many runs
                                 (using gs:SOS[A,G],ls:SOS[A,L],stopAt:Int)
  : BResult[A,G,L] =
    //    println(s"[Sim] $visited  --  $missing")
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



  private def collectMore[A<:HasTaus,G,L](g:G, l:L, t:List[A])
                                         (using gs:SOS[A,G], ls:SOS[A,L]): Either[List[String], S[A,G,L]] =
    var more:S[A,G,L] = none
    // for every g--a->g2
    for (a,g2)<- gs.next(g) do
    //      println(s"\n### doing $a\n[Sim] G $g ")
      if a.isTau then more = add(more,g2,l,t) //a::t)
      else
        // find matching l--a1->* s2
        val tr = SOS.nextWeak(ls,l)
        ///// more = set([])
        val mbMatch = for (a2,l2,l3opt)<-tr if a==a2
          yield l3opt match
            // found l--a->l2 to match g--a->g2
            case None => one(g2,l2,a::t)
            // found l--tau+->l3--a->l2
            case Some(l3) => and( one(g,l3,t) , one(g2,l2,a::t) )
                             //and( one(g,l3,Tau::t) , one(g2,l2,a::Tau::t) )
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

