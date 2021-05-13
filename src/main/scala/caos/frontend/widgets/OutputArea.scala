package caos.frontend.widgets

import org.scalajs.dom
//import org.singlespaced.d3js.Selection

class OutputArea extends Setable[String]:
  type Block = DomElem //Selection[dom.EventTarget]

  var outputs: Block = _

  def init(div: Block): Unit = outputs = div.append("div").attr("class","alertContainer")

  def message(msg:String): Unit = {
    val out = outputs.append("div").attr("class", "alert alert-info")
    for(s <- msg.split('\n')) out.append("p").attr("style","margin-top: 0px;").text(s)
  }

  def error(msg:String): Unit = {
    val out = outputs.append("div").attr("class", "alert alert-danger")
    for(s <- msg.split('\n')) out.append("p").attr("style","margin-top: 0px;").text(s)
  }
  def warning(msg:String): Unit ={
    val out = outputs.append("div").attr("class", "alert alert-warning")
    for(s <- msg.split('\n')) out.append("p").attr("style","margin-top: 0px;").text(s)
  }

  override def setValue(msg: String): Unit = {
    clear()
    if (msg.nonEmpty) {
      val out = outputs.append("div").attr("class", "alert alert-warning")
      for (s <- msg.split('\n'))
        out.append("p").attr("style", "margin-top: 0px;").html(s)
    }}

  def clear(): Unit = outputs.text("")
