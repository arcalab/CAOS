package caos.frontend.widgets

import caos.frontend.Documentation
import caos.frontend.widgets.WidgetInfo.Visualize
import caos.view.{Mermaid, View}
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}

import scala.runtime.Nothing$

/**
 * Created by   on 02/11/2020
 */


class CustomWidget(name:String, divName:String, reload:()=>Unit, errorBox: OutputArea,
                   moreButtons: List[(String,(()=>Unit,String))],doc:Documentation)
  extends Widget[Unit](name,doc) {

  override val get: Unit = ()

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    panelBox(div, visible,buttons=moreButtons.map(kv=>(Left(kv._1) -> kv._2))).append("div")
      .attr("id", divName)

    dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = {(e: MouseEvent) => if(!isVisible) runWidget() }
  }

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit = if(isVisible) runWidget()

  private def runWidget(): Unit =
    try reload()
    catch Widget.checkExceptions(errorBox,name)

}
