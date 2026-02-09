package caos.frontend.widgets

import org.scalajs.dom
//import org.singlespaced.d3js.Selection

class OutputArea extends Setable[String]:
  type Block = DomElem //Selection[dom.EventTarget]

  var outputs: Block = _

  def init(div: Block, hidden: Boolean = false): Unit = outputs =
    if !hidden then div.append("div").attr("class","alertContainer")
    else div.append("div").attr("class","alertContainer").style("display","none")

  def message(msg:String): Unit = addBox(msg,"info")
  def error(msg:String): Unit = addBox(msg,"danger")
  def warning(msg:String): Unit = addBox(msg,"warning")

  override def setValue(msg: String): Unit = {
    clear()
    if (msg.nonEmpty) {
      val out = outputs.append("div").attr("class", "alert alert-info") // set value prints as a message
      for (s <- msg.split("\n\n"))
        out.append("p").attr("style", "margin-top: 0px;").html(s)
    }}

  private def addBox(msg:String, style:String): Unit = // style is "danger", "warning", or "info"
    val rnd = math.random().toString
    val out = outputs.append("div")
      .attr("id",rnd)
      .attr("class", s"alert alert-$style")

    val bt = out.append("div")
      .attr("class","mydiv")
      .append("button")
      .attr("id","close-btn")
      .attr("class","close-btn")
    bt.text("x")
    bt.on("click", () =>
      DomElem(dom.document.getElementById(rnd)).style("display","none"))
    
    for(s <- msg.split("\n\n")) out.append("p").attr("style","margin-top: 0px;").html(s)

  def clear(): Unit =
    outputs.text("")
