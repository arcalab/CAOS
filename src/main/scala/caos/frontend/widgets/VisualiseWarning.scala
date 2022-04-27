package caos.frontend.widgets

import caos.view.View
import com.sun.net.httpserver.Authenticator.Success
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}
import concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

case class VisualiseWarning(text:()=>View, name:String, errorBox: OutputArea)
  extends Widget[Unit](name) {

  private var box: Block = _
  private var txt: Block = _
  protected val divBox = name.replace(' ', '_') + "Box"

  override val get: Unit = () //mermaid()

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    //    box = panelBox(div, visible,buttons=Nil).append("div")
    //      .attr("class","text")
    //      .attr("id", divBox)
    //      .append("pre")
    //      .attr("style","text-align: left;margin: 0;font-size: 1.2rem;")
    //
    //    dom.document.getElementById(name).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
    //      .onclick = {(e: MouseEvent) => if(!isVisible) showText() }
  }

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit =
    showText()

  def showText(): Unit = {
    try {
      val f: Future[String] = Future(text().code)
      f foreach { str =>
        errorBox.warning(str ++ "\nSee Widget Realisability for more information")
      }
      //      val toShow = text().code//view(pre(mermaid()))
      //      box.text(toShow) //.replace("\n","\\n"))
    } catch Widget.checkExceptions(errorBox, name)
  }
}