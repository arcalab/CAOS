package caos.frontend.widgets

import org.scalajs.dom
//import org.singlespaced.d3js.Selection

class OutputArea extends Setable[String]:
  type Block = DomElem //Selection[dom.EventTarget]

  var outputs: Block = _

  def init(div: Block): Unit = outputs = div.append("div").attr("class","alertContainer")

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
    val out = outputs.append("div").attr("class", s"alert alert-$style")
    for(s <- msg.split('\n')) out.append("p").attr("style","margin-top: 0px;").text(s)

  def clear(): Unit =
    outputs.text("")
