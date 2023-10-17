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

  import BError._

  // adapt error message to drop references to "tau"
  override def wrapWeak(act: String): String = act

/** Find a strong bisimulation. */
  def findBisim[A,G,L](g:G,l:L)(using gs:SOS[A,G], ls:SOS[A,L],stopAt:Int=5000): BResult[A,G,L] =
    findWBisim2Aux(Map(),Map((g,l)->Nil),Set(),Nil,1)

  private def findWBisim2Aux[A,G,L](visited:RT[A,G,L],
                                  missing:RT[A,G,L],
                                  triedHash:Set[Int], // to avoid redundant searches
                                  history:List[Int],
                                  i:Int) // to count how many runs
                                 (using gs:SOS[A,G],ls:SOS[A,L],stopAt:Int)
  : BResult[A,G,L] =
    if i >= stopAt /*5000/800000*/ then
      return Left(BEvid(Set(List(Timeout(visited,missing))),triedHash,i))
    missing.headOption match
      // Success!
      case None => Right(visited)

      // Already visited
      case Some(ab,t) if visited contains ab =>
        findWBisim2Aux(visited,missing-ab,triedHash,history,i)

      // Fail: not equally accepting
      case Some(((g:G,l:L),t)) if gs.accepting(g) != ls.accepting(l) =>
        if gs.accepting(g) then
          Left(BEvid(Set(List(CanAccept(t,g,l))),triedHash,i)) else
          Left(BEvid(Set(List(CanAccept(t,l,g))),triedHash,i))

      // traverse steps...
      case Some(((g:G,l:L),t)) =>
        // for every cs1 --a1-> cs1',
        //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]

        // Collect all candidates to add to the bisimulation
        val moreGL = collectMore(g,l,t) match
          case Left(err) => return Left(BEvid(Set(err),triedHash,i))
          case Right(m) => m
        val moreLG = collectMore(l,g,t) match
          case Left(err) => return Left(BEvid(Set(err.map(swap)),triedHash,i))
          case Right(m) => swap(m)
        var more:S[A,G,L] = and( moreGL, moreLG)

        //// Collected all candidates to add to the bisimulation (`more`)
        //// Now we need to prune repeated steps, and try all options (collecting info when one branch fails).

        /// Avoiding recurrent paths...
        val newTry = (missing.keys,more.map(_.keys)).hashCode
        if triedHash contains newTry then
          return Left(BEvid(Set(),triedHash,i))

        // check if, for any m<-more, a bisimulation can be found with `visited + m`
        var failed: Option[BEvid[A,G,L]] = None
        var newTries = triedHash+newTry
        //        var newError = lastError
        var round = i
        var go=1

        while (more.nonEmpty) do
          val m = more.head // next option to try
          findWBisim2Aux(visited + ((g,l)->t), missing++m, newTries, go::history, round+1) match
            case Right(value) =>
              return Right(value)
            case Left(err) =>
              err.msgs.headOption match
                case Some(Timeout(_,_)::_) => return Left(err)
                case _ =>
              newTries ++= err.tried
              round = err.count
              go += 1
              failed = failed match
                case None => Some(err)
                case Some(e@BEvid(msgs,tried,count)) =>
                  Some(BEvid(msgs++err.msgs,newTries,round))
              more -= m

        failed match
          case Some(err) => Left(err)
          case None => Right(visited)


  private def collectMore[A,G,L](g:G, l:L, t:List[A])
                                         (using gs:SOS[A,G], ls:SOS[A,L]): Either[List[BError[A,G,L]], S[A,G,L]] =
    var more:S[A,G,L] = none
    // for every g--a->g2
    for (a,g2)<- gs.next(g) do
        // find matching l--a1->* s2
        val tr = ls.next(l) //SOS.nextWeak(ls,l)
        ///// more = set([])
        val mbMatch = for (a2,l2)<-tr if a==a2
          yield
            // found l--a->l2 to match g--a->g2
            one(g2,l2,a::t)
        if mbMatch.isEmpty then
          return Left(List(CanDo(a,t,g,l)))
        more = and(more , ors(mbMatch)) //mbMatch.flatten
    Right(more)


  ///// utils /////
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

