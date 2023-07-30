package caos.frontend

case class Documentation(private val docs: Documentation.Documents = Map()):
  def add(widget:String, tip:String, body:String) =
    new Documentation(docs+(widget->(tip->body)))
  def add(elements:Iterable[((String,String),String)]) =
    new Documentation(docs++elements.map((xy,z)=>(xy._1,(xy._2,z))).toMap)
  def get(widget:String): Option[(String,String)] =
    docs.get(widget)
  def widgets:Iterable[String] = docs.keys

object Documentation:
  type Documents = Map[String,(String,String)]

