package caos.frontend.widgets

import org.scalajs.dom
import org.scalajs.dom.html
import caos.frontend.widgets.Setable

abstract class CodeWidget[A](title: String, dep: List[Widget[_]])
  extends Widget[A](title) with Setable[String]{

  /// needed to be defined
  protected var input: String
  protected val boxId: String
  protected val buttons: List[(Either[String,String], (()=>Unit,String) )]
  def reload(): Unit
  protected val codemirror:String
  protected val theme:String = "neat"

  //implemented functions from Box and Setable.
  protected var code: scalajs.js.Dynamic = _

  //override def get: String = input

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    val textId = boxId+"Text"
    val inputDiv = panelBox(div, visible /*List("padding-right"->"25pt")*/
      /*, 80*/
      , buttons = buttons)
      .append("div")
      .attr("id", textId)

    inputDiv.append("textarea")
      .attr("id", boxId)
      .attr("name", boxId)
      .attr("class","my-textarea prettyprint lang-java")
      //      .attr("rows", rows.toString)
      .attr("style", "width: 100%; max-width: 100%; min-width: 100%;")

    buildCodeArea(input)

    val realTxt = dom.document.getElementById(textId)
      .childNodes(1).childNodes(0).childNodes(0).asInstanceOf[html.TextArea]
    realTxt.onkeydown = {(e: dom.KeyboardEvent) =>
      if(e.keyCode == 13 && e.shiftKey){e.preventDefault(); reload()}
      else ()
    }
  }

  private def buildCodeArea(txt: String) = {
    val codemirrorJS = scalajs.js.Dynamic.global.CodeMirror
    val lit = scalajs.js.Dynamic.literal(
      lineNumbers = true, matchBrackets = true, theme = theme,
      tabMode="spaces",tabSize=2,id="strangeID"+boxId, mode=codemirror)
    code = codemirrorJS.fromTextArea(dom.document.getElementById(boxId),lit)
    code.setValue(txt)
  }


  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit = {
    val x = code.getValue()
    if (x != null) input = x.toString
  }

  /**
   * sets the value of a given widget, e.g., content text.
   * @param value
   */
  override def setValue(value: String): Unit =
    code.setValue(value)
}
