package caos.sos

abstract class Bisimulation:
  /** Relation of A and B */
  protected type R[A,B] = Set[(A,B)]
  /** Relation + traces (sequencs of Act) */
  protected type RT[Act,A,B] = Map[(A,B),List[Act]] // relation with traces used to get there.
  /** Sets of Relatins+Traces */
  protected type S[Act,A,B] = Set[RT[Act,A,B]]

  /** Evidences that something that went wrong when searching for a bisimulation. */
  case class BEvid[Act,A,B](msgs:Set[List[BError[Act,A,B]]],tried:Set[Int],count:Int)
  /** Error that gives evidence of why a path failed. */
  enum BError[Act,A,B]:
    case Timeout(visited:RT[Act,A,B],missing:RT[Act,A,B])
    case CanDo(act:Act,trace:List[Act],a:A|B,b:A|B)
    case CanAccept(trace:List[Act],a:A|B,b:A|B)
  def swap[Act,A,B](b:BError[Act,A,B]): BError[Act,B,A] = b match
      case BError.Timeout(v,m)         => BError.Timeout(swap(v),swap(m))
      case BError.CanDo(act,trace,a,b) => BError.CanDo(act,trace,a,b)
      case BError.CanAccept(trace,a,b) => BError.CanAccept(trace,a,b)
  def swap[Act,A,B](rt:RT[Act,A,B]): RT[Act,B,A] =
    for (k,v) <- rt yield ((k._2,k._1) -> v)

  /** Result from a bisimulation search: either some evidence of what went wrong, or a bisimulation */
  type BResult[Act,A,B] = Either[BEvid[Act,A,B],RT[Act,A,B]]

  /** Pretty printing a given error message from the bisimulation search. */
  def pp[Act,A,B](b: BError[Act,A,B], showA:A=>String, showB:B=>String): String =
    def show(ab:A|B): String = // hack because of type erasure - prone to errors
      try showA(ab.asInstanceOf[A])
      catch {case _ => showB(ab.asInstanceOf[B])}
      ///// fails because of type erasure
      //    ab match
      //      case a:A => showA(a)
      //      case b:B => showB(b)
    b match
      case BError.Timeout(v, m) if v.isEmpty && m.isEmpty => s"Failed to start searching - maybe found an unbounded loop"
      case BError.Timeout(v, m) => s"Timeout\n   + visited: $v\n   + missing: $m"
      case BError.CanDo(act, trace, a, b) =>
          s"after ${if trace.isEmpty then "[]" else trace.reverse.mkString(",")}\n   + ${
            show(a)} can do $act\n   + ${
            show(b)} cannot do τ*,$act"
      case BError.CanAccept(trace, a, b) =>
        s"after ${if trace.isEmpty then "[]" else trace.reverse.mkString(",")}\n   + ${
          show(a)} is accepting\n   + ${
          show(b)} is not"

  /** Pretty printing bisimulation results. */
  def pp[A,G,L](res: BResult[A,G,L],
                showG:G=>String = (_:G).toString,
                showL:L=>String = (_:L).toString): String = res match
    case Left(err:BEvid[A,G,L]) => "Not bisimilar:"+err.msgs.map(m => m.map("\n - "+pp(_,showG,showL)).mkString).mkString("\n---")
    case Right(rel) => "Found bisimulation:\n"+pp[A,G,L](rel,showG,showL)
                       //println(rel.map(p=>s"- ${p._1}   <->   ${p._2}").mkString("\n"))

  /** Pretty printing bisimulations. */
  def pp[A,G,L](rel:RT[A,G,L],
                showG: G=>String,
                showL: L=>String): String =
    val strs = rel.toList.map((xy,t)=>
      (t.size,showG(xy._1),showL(xy._2),
        if t.isEmpty then "init" else t.mkString(",")))
      .toList.sorted
    val max = strs.map(_._2.size).max
    val strs2 = strs.map((_,x,y,t)=>(x+(" "*(max-x.size)),y,t))
    strs2.map(p=>s"- ${p._1}  <->  ${p._2}  @ ${p._3}").mkString("\n")

  /** Find a strong bisimulation and returns an explanation */
  def findBisimPP[A,G,L](g:G, l:L,
                         showG: G => String = (_: G).toString,
                         showL: L => String = (_: L).toString)
                        (using gs:SOS[A,G], ls:SOS[A,L], stopAt:Int=5000): String =
    pp(findBisim(g,l),showG,showL)

  def findBisim[A,G,L](g:G,l:L)(using gs:SOS[A,G], ls:SOS[A,L],stopAt:Int=5000): BResult[A,G,L]