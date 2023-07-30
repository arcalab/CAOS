package caos.frontend.widgets

import WidgetInfo.Visualize
import caos.frontend.Documentation
import caos.view.{Mermaid, View}
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}

import scala.runtime.Nothing$

/**
 * Created by   on 02/11/2020
 */


class VisualiseText(text:()=>View, name:String, errorBox: OutputArea, doc:Documentation)
  extends Widget[Unit](name,doc) {

  private var box:Block = _
  private var txt:Block = _
  protected val divBox = name.replace(' ','_') + "Box"

  override val get: Unit = () //mermaid()

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    box = panelBox(div, visible,buttons=Nil).append("div")
      .attr("class","text")
      .attr("id", divBox)
      .append("pre")
      .attr("style","text-align: left;margin: 0;font-size: 1.2rem;")

    dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = {(e: MouseEvent) => if(!isVisible) showText() }
  }

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit = if(isVisible) showText()

  def showText():Unit = {
    try {
      val toShow = text().code//view(pre(mermaid()))
      box.text(toShow) //.replace("\n","\\n"))
    } catch Widget.checkExceptions(errorBox,name)
  }

}
