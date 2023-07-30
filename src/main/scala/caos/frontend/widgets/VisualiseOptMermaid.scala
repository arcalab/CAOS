package caos.frontend.widgets

import caos.frontend.Documentation
import caos.view.OptionView
import caos.view.OptionView.*
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}


/**
 * Created by   on 13/07/2021
 */

class VisualiseOptMermaid(mermaid:()=>OptionView,name:String, errorBox: OutputArea,doc:Documentation)
  extends Widget[Unit](name,doc) {

  val diagram:String = ""
  protected var box:Block = _
  protected val svgBox: String = titleId+"Svg" // fix(name) + "Svg"
  protected val divBox: String = titleId+"Box" // fix(name) + "Box"

  protected def fix(s:String): String = s
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
      .append("div") // outside box to center grid
      .style("text-align","center")
      .append("div") // inside box to place each "title+mermaid" (option)
      //.style("display", "flex")
      //.style("justify-content", "flex-start")
      .style("padding","5px 0px 5px 0px")
      .style("display", "inline-flex")
      .style("flex-wrap", "wrap")
    dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = {(_: MouseEvent) => if(!isVisible) showOptions() }
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
    } catch Widget.checkExceptions(errorBox,this.name)

  def showOption(name:String,code:String):Unit = {
    try {
      val namefix = fix(name)
      val mbox = box.append("div")
        .style("text-align","center")
      mbox.append("h4")
        .text(s"$name")
      mbox.append("div")
        .attr("class","mermaid")
        .attr("id", divBox+namefix)
        .style("text-align","center")
        .append("div").attr("id",svgBox+namefix)
      val mermaidJs = MermaidJS(code.replaceAll("\\\\","\\\\\\\\"),
                                divBox+namefix,svgBox+namefix)
      scalajs.js.eval(mermaidJs)
    } catch Widget.checkExceptions(errorBox,this.name)
  }

}