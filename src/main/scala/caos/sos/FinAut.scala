package caos.sos

import scala.annotation.tailrec

object FinAut:

  /** Structure to represent nondeterministic finita automata.
   * These are similar to the SOS structure, but:
   *  - explicitly store a collection of states and edges (instead of having rules for the next state)
   *  - explicitly include initial and final states
   *
   * @param s all states
   * @param s0 initial states
   * @param e all edges
   * @param f final states
   * @tparam A actions
   * @tparam S states
   */
  case class NFA[A,S](s:Set[S],s0: Set[S], e: Set[(S,A,S)], f: Set[S])

  /**
   * Traverses an SOS, compiling an NFA in the process, traversing at most `max` edges
   * @param sos SOS to traverse
   * @param s0 initial states to consider
   * @param max maximum edges traversed
   * @tparam A actions
   * @tparam S states
   * @return A compiled NFA and a boolean indicating if the traversal was complete
   */
  def sosToNFA[A,S](sos:SOS[A,S], s0:Set[S], max:Int=5000): (NFA[A,S],Boolean) =
    @tailrec
    def aux(next:Set[S],done:Set[S],finl:Set[S],edges:Set[(S,A,S)], limit:Int)
        : (Set[S],Set[S],Set[(S,A,S)],Boolean) = // states, final-states, edges, finished
      if limit <=0 then
        return (done,finl,edges,false)
      next.headOption match
        case None =>
          (done, finl, edges, true)
        case Some(st) if done contains st =>
          aux(next-st,done,finl,edges,limit)
        case Some(st) => //visiting new state
          val more = sos.next(st)
          val finl2 = if sos.accepting(st) then finl+st else finl
          aux((next-st)++more.map(_._2), done+st, finl2,
              edges ++ more.map(xy=>(st,xy._1,xy._2)),limit-more.size)

    val (s2,f2,e2,done) = aux(s0, Set(), Set(), Set(), max)
    NFA(s=s2,s0=s0,e=e2,f=f2) -> done

  /**
   * Converts a static NFA to an SOS that calculates the next state from the NFA
   * @param nfa automata with the pre-computed edges
   * @tparam A actions
   * @tparam S states
   * @return an SOS that mimics the pre-computed NFA and a set of initial states
   */
  def nfaToSOS[A,S](nfa:NFA[A,S]): (SOS[A,S],Set[S]) =
    val mapE: Map[S,Set[(A,S)]] = nfa.e
      .groupBy(sas => sas._1) // from |-> Set(from,act,to)
      .map(kv => kv._1 -> (kv._2.map(edge => edge._2 -> edge._3)))
    val sos = new SOS[A,S] {
      override def accepting(s: S): Boolean = nfa.f contains s
      override def next[A2 >: A](s: S): Set[(A2, S)] =
        mapE.getOrElse(s,Set()).asInstanceOf[Set[(A2,S)]]
    }
    sos -> nfa.s0


  /**
   * Makes an SOS deterministic, without traversing all states
   * (just looking at a given state at a time)
   * @param sos SOS to be determinised
   * @tparam Act actions
   * @tparam St states
   * @return a new deterministic SOS
   */
  def detSOS[Act,St](sos:SOS[Act,St]): SOS[Act,Set[St]] = new SOS {
    override def accepting(s: Set[St]): Boolean =
      s.exists(sos.accepting)

    /** Set of next states `State` and corresponding labels `Act`. */
    override def next[A >: Act](ss: Set[St]): Set[(A, Set[St])] =
      ss.flatMap(sos.next)
        .groupBy(_._1)
        .map(kv => kv._1 -> kv._2.map(_._2))
        .toSet
  }

  /** WiP - mean to compute the deterministic finite automata from a nondeterministic one.
   *
   * @param nfa non-deterministic finite automata
   * @tparam Act actions
   * @tparam St state
   * @return deterministic finite automata (using an NFA structure)
   */
  def detNFA[Act,St](nfa:NFA[Act,St]): NFA[Act,Set[St]] =
    val nMap = nfa.e.groupBy(_._1)
    @tailrec
    def aux(res:NFA[Act,Set[St]], done:Set[Set[St]], nx:Set[Set[St]]): NFA[Act,Set[St]] =
      nx.headOption match
        case None => res
        case Some(st) if done(st) => aux(res,done,nx-st)
        case Some(st) =>
          val more = st.flatMap(s => nMap.getOrElse(s,Set()))
            .groupBy(_._2) // act -> set of edges that use act from st
            .map((act,edges) => (st,act,edges.map(_._3)))
            .toSet
          val newS0 = if st.intersect(nfa.s0).nonEmpty then Set(st) else Set[Set[St]]()
          val newF = if st.intersect(nfa.f).nonEmpty then Set(st) else Set[Set[St]]()
          aux(res.copy(e = res.e ++ more, s0 = res.s0 ++ newS0, s = res.s+st, f = res.f ++ newF),
            done+st,nx-st)

    aux(NFA(Set(),Set(),Set(),Set()),Set(),Set(nfa.s0))


  /**
   * Reverses the edges of an NFA
   * @param nfa to be reversed
   * @tparam A actions
   * @tparam S states
   * @return a new NFA with reversed edges (and initial-final states swapped)
   */
  def revNFA[A,S](nfa:NFA[A,S]): NFA[A,S] =
   val revEdges = for (s1,a,s2) <- nfa.e yield (s2,a,s1)
   NFA(nfa.s,nfa.f,revEdges,nfa.s0)

  /**
   * (WiP) Minimises an NFA using Brzozowski's algorithm, by reversing and minimising twice.
   * Still has problems: the initial states after determinisation are not correct.
   * @param nfa NFA to be determinised
   * @tparam A actions
   * @tparam S states
   * @return an SOS with a minimal deterministic automata
   */
  @deprecated
  def minNFADep[A,S](nfa:NFA[A,S], max:Int=5000): (SOS[A,Set[Set[S]]],Set[Set[Set[S]]],Boolean) =
    val r1:NFA[A,S]               = revNFA(nfa) // first reverse
    val (r2:SOS[A,S], s02:Set[S]) = nfaToSOS(r1) // then get the SOS (lazy NFA)
    val r3:SOS[A,Set[S]]          = detSOS(r2) // then determinise the SOS (no need for initial state)
    val s03:Set[Set[S]]           = s02.map(s => Set(s)) // WRONG: initial states of the deterministic SOS (need to include all that contain initial states)
    val (r4:NFA[A,Set[S]],done)   = sosToNFA(r3,s03,max) // reversed NFA (now DFA)
    val r5:NFA[A,Set[S]]          = revNFA(r4) // second reverse
    val (r6:SOS[A,Set[S]], s06:Set[Set[S]]) = nfaToSOS(r5) // then get the 2nd SOS (lazy NFA)
    val r7: SOS[A,Set[Set[S]]]    = detSOS(r6) // then determinise the end SOS (no need for initial state)
    val s07:Set[Set[Set[S]]]      = s06.map(s => Set(s)) // WRONG: initial states of the 2nd deterministic SOS
    (r7,s07,done)

  /**
   * WiP: Minimises an SOS, using the minimisation of its NFA based on Brzozowski's algorithm
   * (does not work yet - using a broken minNFA function)
   * @param sos to be minimised
   * @param s initial state
   * @param max maximum edges traversed when pre-computing NFAs (twice)
   * @tparam A actions
   * @tparam S states
   * @return a minimal SOS, its initial states, and a boolean indicating if the traversals were complete
   */
  @deprecated
  def minSOSBr[A,S](sos:SOS[A,S], s:S, max:Int=5000): (SOS[A,Set[Set[S]]],Set[Set[Set[S]]],Boolean) =
    val (nfa,done) = sosToNFA(sos,Set(s),max)
    val (sos2,s02,done2) = minNFADep(nfa,max)
    (sos2,s02,done && done2)


  /** Computes the minimal DFA (trace equiv) from a given NFA, using Hopcrofts's algorithm
   * (https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft's_algorithm)
   *
   * @param nfa to be determinised and minimized
   * @tparam Act action type
   * @tparam S state type
   * @return a minimal DFA (using an NFA structure)
   */
  def minNFA[Act,S](nfa: NFA[Act,S]): NFA[Act,Set[S]] =
    val part = partitionNFA(nfa)
    val mpart = part.flatMap(es => es.map( e => e -> es)).toMap
    val s   = part.filter(_.intersect(nfa.s).nonEmpty)
    val ini = part.filter(_.intersect(nfa.s0).nonEmpty)
    val fin = part.filter(_.intersect(nfa.f).nonEmpty)
    val edg = nfa.e.map(e => (mpart(e._1),e._2,mpart(e._3))) //error if not all edges are in part.
    NFA(s,ini,edg,fin)

  /**
   * Minimises an SOS semantics, using an intermediate NFA with all explicit states, using Hopcroft's algorithm
   * (https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft's_algorithm)
   * @param sss Input SOS to be minimised
   * @param s0 initial states
   * @tparam Act actions
   * @tparam S states
   * @return triple with: initial states, minimal SOS, and a boolean indicating if all states were traversed
   */
  def minSOS[Act,S](sss:SOS[Act,S],s0:Set[S], max:Int = 5000): (Set[Set[S]],SOS[Act,Set[S]],Boolean) =
    val (nfa, done) = FinAut.sosToNFA(sss, s0, max)
    val mdfa = FinAut.minNFA(nfa)
    val (sos, ini) = FinAut.nfaToSOS(mdfa)
    (ini,sos,done)

  /**
   * Uses the Hopcroft's algorithm (https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft's_algorithm)
   * to calculate sets of indistinguishable states (using trace equivalence)
   * @param nfa Non-deterministic finite automata, to be traversed
   * @tparam Act actions
   * @tparam S states
   * @return partition, i.e., set of sets of equivalent states
   */
  def partitionNFA[Act,S](nfa: NFA[Act,S]): Set[Set[S]] = { //NFA[Act,Set[S]] = {
    var p = Set(nfa.f, nfa.s -- nfa.f)
    var w = Set(nfa.f, nfa.s -- nfa.f)
    val prev = nfa.e.groupBy(_._3)
    while w.nonEmpty do
      val a = w.head
      w -= a
      //println(s"# a[w]: ${a.mkString("{",",","}")} [${w.map(_.mkString("{",",","}")).mkString(";")}]")
      val toA = for qa <- a; edge <- prev.getOrElse(qa,Set()) yield edge
      for (c,edges) <- toA.groupBy(_._2) do
        val x = edges.map(_._1)
        //println(s"### x: ${x.mkString(",")}")
        for y <- p do
          //println(s"##### y[p]: ${y.mkString(",")} [${p.map(_.mkString(",")).mkString(";")}]")
          val y1 = x intersect y
          val y2 = y -- x
          //println(s"##### y1/y2: ${y1.mkString(",")} / ${y2.mkString(",")}")
          if y1.nonEmpty &&  y2.nonEmpty then
            p = (p - y) + y1 + y2
            //println(s"####### new p: ${p.map(_.mkString(",")).mkString(";")}")
            if w(y) then
              w = w - y + y1 + y2
            else
              if y1.size <= y2.size then
                w += y1
              else
                w += y2
            //println(s"####### new w: ${w.map(_.mkString(",")).mkString(";")}")
    p
  }

/* Hopcroft's algorithm, from wikipedia (by Xu),  based on partition refinement
P := {F, Q \ F} -- sets of states being refined
W := {F, Q \ F} -- sets of states to be analyzed

while (W is not empty) do
    choose and remove a set A from W
    for each c s.t. X -c-> A do (A,X sets of states)
        for each set Y in P
          calculate Y1 = X ∩ Y and Y2 = Y \ X
          if both are non-empty:
            P = P - Y + Y1 + Y2
            if Y in W
                W = W - Y + Y1 + Y2
            else
                if |Y1| <= |Y2|
                    W +=  Y1
                else
                    W += Y2
   */
