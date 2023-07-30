package caos.frontend.widgets


import WidgetInfo.Visualize
import caos.frontend.Documentation
import caos.frontend.widgets.Widget.Helper
import caos.view.{Mermaid, View}
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, document, html}

import scala.runtime.Nothing$

/**
 * Created by   on 02/11/2020
 */


class VisualiseMermaid(mermaid:()=>View,name:String, errorBox: OutputArea, doc:Documentation)
  extends Widget[Unit](name,doc) {

  val diagram: String = ""
  protected var box: Block = _
  protected val svgBox = titleId + "Svg" //fix(name) + "Svg"
  protected val divBox = titleId + "Box" //fix(name) + "Box"

  protected def fix(s: String) = s
    .replace(' ', '_')
    .replace('(', '_')
    .replace(')', '_')

  override val get: Unit = () //mermaid()

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    box = panelBox(div, visible, buttons = List(
      Right("download") -> (() => Utils.downloadSvg(svgBox), "Download SVG")
    )).append("div")
      .attr("class", "mermaid")
      .attr("id", divBox)
      .style("text-align", "center")
      .append("div").attr("id", svgBox)

    dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = { (e: MouseEvent) => if (!isVisible) showMermaid() }
  }

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit = if (isVisible) showMermaid()

  def showMermaid(): Unit =
    try {
      val diagram = mermaid().code
      showMermaid(diagram)
    } catch Widget.checkExceptions(errorBox,name)

  def showMermaid(diagram:String):Unit = {
    try {
      // clean and repeat the boostrap process
      box.text("") // not always working when mermaid() throws an error.
      box.append("div")
        .attr("class", "mermaid")
        .attr("id", divBox)
        .style("text-align", "center")
        .append("div").attr("id", svgBox)
      val flush = MermaidJS(s"graph TD\n ",divBox,svgBox) // flush!!!
      scalajs.js.eval(flush) // harder flush to clean when mermaid() throws an error.

      val diagramFixed = diagram
        .replaceAll("\\\\","\\\\\\\\")
      val mermaidJs = MermaidJS(diagramFixed,divBox,svgBox)
//      println(s"Evaluating mermaid:\n$mermaidJs")
      scalajs.js.eval(mermaidJs)
    } catch Widget.checkExceptions(errorBox,name)
  }

}
