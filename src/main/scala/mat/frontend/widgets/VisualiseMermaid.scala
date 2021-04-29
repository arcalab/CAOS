package mat.frontend.widgets


import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}

import scala.runtime.Nothing$

/**
 * Created by guillecledou on 02/11/2020
 */


class VisualiseMermaid(mermaid: ()=>String, parentName:String, errorBox: OutputArea)
  extends Box[Unit]("Sequence diagram of the choreography", List()) {

  val diagram:String = ""
  private var box:Block = _

  override def get: Unit = {} //mermaid()

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    box = panelBox(div, visible,buttons=List(
      Right("download")-> (() => Utils.downloadSvg("svgChoreography"), "Download SVG")
    )).append("div")
      .attr("class","mermaid")
      .attr("id", "choreographyBox")
      .style("text-align","center")
      .append("div").attr("id","svgChoreography")

    dom.document.getElementById("Sequence diagram of the choreography").firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = {(e: MouseEvent) => if(!isVisible) showChoreo() }
  }

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit = if(isVisible) showChoreo()

  def showChoreo():Unit = {
    try {
      val diagram = mermaid()
      val mermaidJs = MermaidJS(diagram,parentName,"svg"+parentName)
      scalajs.js.eval(mermaidJs)
    } catch Box.checkExceptions(errorBox)
  }

}
