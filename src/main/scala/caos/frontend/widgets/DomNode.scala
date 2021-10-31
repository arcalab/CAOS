package caos.frontend.widgets

import org.scalajs.dom
import org.scalajs.dom.{document, html}

class DomNode(node: dom.Node) {
  def append(name:String): DomElem = {
    val el = name match {
      case "svg" | "svg:path" | "path" | "circle" | "g" | "rect" | "defs" | "marker" =>
        document.createElementNS("http://www.w3.org/2000/svg",name)
      case _ =>
        document.createElement(name)
    }
    node.appendChild(el)
    new DomElem(el)
  }

  def text(txt:String): DomNode = {
    node.textContent = txt
    this
  }
}

class DomElem(elem: dom.Element) extends DomNode(elem) {
  def attr(name:String, value:String): DomElem = {
    elem.setAttribute(name,value)
    this
  }
  def attr(name:String, value:Double): DomElem = {
    elem.setAttribute(name,value.toString)
    this
  }

  def resetStyle(): DomElem = {
    elem.setAttribute("style","")
    this
  }
  def style(str:String): DomElem = {
    elem.getAttribute("style") match {
      case null => elem.setAttribute("style", str)
      case "" => elem.setAttribute("style", str)
      case str2: String =>
        elem.setAttribute("style", str2 + str)
    }
    this
  }
  def style(attr:String, value:String): DomElem =
    style(s"$attr: $value;")

  def html(html:String): DomElem = {
    elem.innerHTML = html
    this
  }

  def textEl(txt:String): DomElem = {
    elem.textContent = txt
    this
  }

  def on(`type`: String, f:()=>Unit): Unit = {
    //    org.scalajs.dom.document.onclick
    elem.addEventListener(`type`, {(_:dom.Event) => f()})
  }

  // does not seem to work
  def deleteChildren: DomElem = {
    val ns = elem.childNodes
    for (i <- 0 to ns.length) {
      val node: dom.Node = ns(i)
      elem.removeChild(node)
    }
    this
  }

  // does not seem to work
  def deleteAll(el:String): DomElem = {
    val ls = elem.querySelectorAll(el)
    for (i <- 0 until ls.length) {
      val nextList = ls(i)
      while (nextList.hasChildNodes())
        ls(i).removeChild(nextList.childNodes(0))
    }
    this
  }

  def select(selector:String): DomElem =
    new DomElem(elem.querySelector(selector))
}

object DomNode {
  def select(div: html.Div): DomElem = {
    new DomElem(div)
  }
  def select(div: String): DomElem = {
    val c = document.getElementById(div)
    new DomElem(c)
  }
}

