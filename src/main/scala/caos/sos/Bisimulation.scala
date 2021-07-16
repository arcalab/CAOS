package caos.sos

abstract class Bisimulation:
  /** Relation of A and B */
  protected type R[A,B] = Set[(A,B)]
  /** Relation + traces (sequencs of Act) */
  protected type RT[Act,A,B] = Map[(A,B),List[Act]] // relation with traces used to get there.
  /** Sets of Relatins+Traces */
  protected type S[Act,A,B] = Set[RT[Act,A,B]]

  /** Evidences that something that went wrong when searching for a bisimulation. */
  case class BEvid(msgs:Set[List[String]],tried:Set[Int],count:Int)
  /** Result from a bisimulation search: either some evidence of what went wrong, or a bisimulation */
  type BResult[Act,A,B] = Either[BEvid,RT[Act,A,B]]

  /** Pretty printing bisimulation results. */
  def pp[A,G,L](res: BResult[A,G,L]): String = res match
    case Left(err:BEvid) => "Not a bisim."+err.msgs.map(m => m.map("\n - "+_).mkString).mkString("\n---")
    case Right(rel) => pp[A,G,L](rel) //println(rel.map(p=>s"- ${p._1}   <->   ${p._2}").mkString("\n"))

  /** Pretty printing bisimulations. */
  def pp[A,G,L](rel:RT[A,G,L]): String =
    val strs = rel.toList.map((xy,t)=>
      (t.size,xy._1.toString,xy._2.toString,
        if t.isEmpty then "init" else t.mkString(",")))
      .toList.sorted
    val max = strs.map(_._2.size).max
    val strs2 = strs.map((_,x,y,t)=>(x+(" "*(max-x.size)),y,t))
    strs2.map(p=>s"- ${p._1}  <->  ${p._2}  @ ${p._3}").mkString("\n")

  /** Find a strong bisimulation and returns an explanation */
  def findBisimPP[A,G,L](g:G,l:L)(using gs:SOS[A,G], ls:SOS[A,L],stopAt:Int=5000): String =
    pp(findBisim(g,l))

  def findBisim[A,G,L](g:G,l:L)(using gs:SOS[A,G], ls:SOS[A,L],stopAt:Int=5000): BResult[A,G,L]