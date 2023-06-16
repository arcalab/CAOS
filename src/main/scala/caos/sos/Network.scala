package caos.sos

import caos.sos.SOS

/** A network is a sequence of participants, each with a local semantics.
 * The provided `sos` function builds a new SOS semantics from 3 core parameters:
 * (1) how to synchronise local actions, (2) how to relabel valid combinations of
 * actions, and (3) the SOS semantics for each of the local participants. */

object Network:
  /** State of a network includes the state of the participants and the state of the network. */
  case class State[LSt,NSt](parts:List[LSt], netSt:NSt):
    override def toString: String =
      s"${parts.mkString("  ---  ")}  ---  $netSt}"

  /**
   * Produces an SOS semantics for a network of participants
   *
   * @param sync     Filter possible combinations of available transitions from local components and update the network state;
   * @param relabel  Rename a combination of labels from local components into a new global label;
   * @param localSOS SOS semantics of local components;
   * @tparam NAct the type of the labels for the network;
   * @tparam LAct the type of the labels for local components;
   * @tparam LSt  the type of a local component;
   * @tparam NSt  the type of the extra state information for the network, e.g., pending messages
   * @return a new SOS semantics for the network of participants
   */
  def sos[NAct, LAct, LSt, NSt]( //project: GSpec => List[LSpec],
                                 sync: (List[Set[LAct]], NSt) => Set[(List[Option[LAct]], NSt)],
                                 relabel: List[Option[LAct]] => NAct,
                                 localSOS: SOS[LAct, LSt]): SOS[NAct, State[LSt, NSt]] =
    sos(sync,relabel, nst => LazyList.continually(localSOS)) // infinite list of localSOS - to be "zipped"


  /**
   * Produces an SOS semantics for a network of participants
//   * @param project - how to project a global specification into local components;
   * @param sync  Filter possible combinations of available transitions from local components and update the network state;
   * @param relabel  Rename a combination of labels from local components into a new global label;
   * @param localSOSs  SOS semantics of EACH local components (possibly infinite list to be "zipped";
   * @tparam NAct  the type of the labels for the network;
   * @tparam LAct  the type of the labels for local components;
//   * @tparam GSpec  the type of a global specification, used to produce local components via projection;
   * @tparam LSt  the type of a local component;
   * @tparam NSt  the type of the extra state information for the network, e.g., pending messages
   * @return a new SOS semantics for the network of participants
   */
  def sos[NAct,LAct,LSt,NSt]( //project: GSpec => List[LSpec],
                              sync: (List[Set[LAct]],NSt) => Set[(List[Option[LAct]],NSt)],
                              relabel: List[Option[LAct]] => NAct,
                              localSOSs: NSt => Iterable[SOS[LAct,LSt]]): SOS[NAct,State[LSt,NSt]] =
    new SOS[NAct,State[LSt,NSt]]:
      type St = State[LSt,NSt]
      /** Next possible network actions from a given network state. */
      def next[A>:NAct](s:St): Set[(A,St)] =
        val prevs = s.parts
        val news  = prevs.zip(localSOSs(s.netSt)).map((st,sos)=>sos.next(st))
        val synced = sync(news.map(set=>set.map(_._1)), s.netSt)
        //println(s"[CAOSNet] news: $news")
        //println(s"[CAOSNet] synced: $synced")
        val res =
          for (validComb,netSt2) <- synced yield
            val globalAct = relabel(validComb)
            val localSpecs = findLSpec(prevs,news,validComb)
            //println(s"[CAOSNet] localSpec: $localSpecs")
            val combined = combine(localSpecs)
            //println(s"[CAOSNet] combined: $combined")
            for lspecs <- combined yield
              globalAct -> State(lspecs,netSt2)
        res.flatten

      /** Auxilary: E.g. [{a,b},{c,d,e}] --> { [a,c] [a,d] [a,e] [b,c] [b,d] [b,e]} */
      private def combine[A](lst: List[Set[A]]): Set[List[A]] = lst match
        case Nil => Set(Nil)
        case set :: tail =>
          val lst2 = combine(tail)
          set.flatMap(a => lst2.map(a :: _))

      /** Find local specifications for a given selection of actions. */
      private def findLSpec[A,S](prevs:List[S],
                                 news:List[Set[(A,S)]],
                                 acts:List[Option[A]]): List[Set[S]] =
        for ((p,n),a) <- prevs.zip(news).zip(acts) yield
          a match
            case None => Set(p)
            case Some(act) => n.filter(_._1==act).map(_._2)

      
      // Ignores the state of the network, and checks if all participants are
      // either in an accepting state or can reach one by a Tau step.
      override def accepting(s:St): Boolean =
       //s.parts.forall(c => SOS.byTau(localSOS,c).exists(localSOS.accepting))
       s.parts.zip(localSOSs(s.netSt)).forall((c,sos) => SOS.byTau(sos,c).exists(sos.accepting))

/*
Intuition for the "next" construction:
s.parts
  - [ LSpec ]          --> prevs
map localSOS.next
  - [ {LAct x LSpec} ] --> news
map map fst
  - [ {LAct} ]
sync(_,netSt)
  - {  [LAct?] x NSt  }
{ relabel&id x id }
  - {  NAct x [LAct?] x NSt }
map id x findLSpec(prevs x news x _) x id
  - {  NAct x [{LSpec}] x NSt }
map id x combine x id
  - {  NAct x {[LSpec]} x NSt }
map repack
  - {  {NAct x [LSpec] x NSt} }
map id x State
  - {  {NAct x State} }
flatten
  - {  NAct x State }
*/