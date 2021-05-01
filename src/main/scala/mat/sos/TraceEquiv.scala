package mat.sos

object TraceEquiv:

  def apply[A,S1,S2](s1:S1,s2:S2, sos1:SOS[A,S1], sos2:SOS[A,S2]) =
    pp(findTraceEq(Set(s1),Set(s2),"",10000)(using sos1,sos2))
  //  case class TEvid[A,B](t1:A,t2:B)
  type TRes = Option[Option[String]]

  def pp(r:TRes):String = r match
    case None => "Trace equivalence failed: bound reached"
    case Some(None) => "Are trace equivalent"
    case Some(Some(reason)) => s"Not trace equivalent: $reason"

  def findTraceEq[A,S1,S2](s1:Set[S1],s2:Set[S2],done:String,maxActions:Int)
                          (using sos1:SOS[A,S1],sos2:SOS[A,S2]): TRes =
    if maxActions<=0 then return None

    val n1 = toMap(s1.flatMap(sos1.next))
    val n2 = toMap(s2.flatMap(sos2.next))
    if n1.keySet!=n2.keySet then
      Some(Some(s"after [$done] options differ: [${n1.keySet.mkString(",")}] vs. [${n2.keySet.mkString(",")}]"))
    else
      for a<-n1.keys do
        val res = findTraceEq(n1(a),n2(a),done+"."+a, maxActions-1)
        if res!=Some(None) then return res
      Some(None)


  private def toMap[A,B](set: Set[(A,B)]): Map[A,Set[B]] = set.headOption match
    case None => Map()
    case Some((a,b)) =>
      val m = toMap(set-((a,b)))
      if m contains a then m+(a-> (m(a)+b)) else m+(a->Set(b))
