package caos.frontend.widgets

import caos.view.OptionView
import caos.view.OptionView._
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}


/**
 * Created by   on 13/07/2021
 */

class VisualiseOptMermaid(mermaid:()=>OptionView,name:String, errorBox: OutputArea)
  extends Box[Unit](name, List()) {

  val diagram:String = ""
  protected var box:Block = _
  protected val svgBox = fix(name) + "Svg"
  protected val divBox = fix(name) + "Box"

  protected def fix(s:String) = s
    .replace(' ','_')
    .replace('(','_')
    .replace(')','_')
    .replace(':','_')

  override val get: Unit = () //mermaid()

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    box = panelBox(div, visible,buttons=List())
      .append("div")
      //.style("display", "flex")
      //.style("justify-content", "flex-start")
      .style("padding","5px 1px 5px 15px")
    dom.document.getElementById(name).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = {(e: MouseEvent) => if(!isVisible) showOptions() }
  }



  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit = if(isVisible) showOptions()

  def showOptions():Unit =
    try {
      box.text("")
      for ((name,code)<-mermaid().options) do
        showOption(name,code)
    } catch Box.checkExceptions(errorBox)

  def showOption(name:String,code:String):Unit = {
    try {
      val namefix = fix(name)
      val mbox = box.append("div")
        .style("text-align","center")
      mbox.append("h4")
        .text(s"${name}")
      mbox.append("div")
        .attr("class","mermaid")
        .attr("id", divBox+namefix)
        .style("text-align","center")
        .append("div").attr("id",svgBox+namefix)
      val mermaidJs = MermaidJS(code,divBox+namefix,svgBox+namefix)
      scalajs.js.eval(mermaidJs)
    } catch Box.checkExceptions(errorBox,this.name)
  }

}