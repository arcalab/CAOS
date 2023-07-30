package caos.frontend.widgets

import caos.frontend.Documentation
import caos.frontend.widgets.WidgetInfo.Simulate
import caos.sos.HasTaus
import caos.sos.SOS
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, document, html}

/**
 * Created by   on 16/03/2021
 */

class Explore[Act,St](init:()=>St,sos:SOS[Act,St],vS:St=>String,vA:Act=>String,
                      name:String, errorBox: OutputArea,
                      doc:Documentation)
  extends Widget[Unit](name,doc) {

  private var container:Block = _
  protected val svgBox: String = titleId+"Svg" // fix(name) + "Svg"
  protected val divBox: String = titleId+"Box" // fix(name) + "Box"

  private def fix(s:String) = s
    .replace(' ','_')
    .replace('(','_')
    .replace(')','_')

  override def get: Unit = ()

  protected var root: St = _
  protected var tree: Map[St,Set[(Act,St)]] = Map()

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean=false): Unit = {
    val box = panelBox(div, visible, buttons = List(
      Right("refresh") -> (() =>
        update(), "Explore next actions of the current program")
    ))
    dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = { (_: MouseEvent) => if(!isVisible) initialise() }

    container = box.append("div")
      .style("display:inline; width:100%;")
    container.append("div")
      .attr("class","mermaid")
      .attr("id", divBox)
      .style("text-align","center")
      .append("div").attr("id",svgBox)
  }
  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit =
    if(isVisible) initialise()

  def initialise():Unit = try {
    root = init()
    tree = Map(root -> (sos.next(root)))
    updateDiagram()
  } catch Widget.checkExceptions(errorBox,name)

  protected def expandState(st:St):Unit = try {
    tree += st -> (sos.next(st))
    updateDiagram()
    document.location.replace(s"#${mkStId(st)}")
//    updateSimulationSteps((None:: traceActs.map(Some(_))).zip(traceStx))
  } catch Widget.checkExceptions(errorBox,name)

  protected def updateDiagram():Unit = try {
    val (mermaid,leafs) = mkMermaid
    //println(s"--- About to view $st")
    val mermaidFixed = mermaid
      .replaceAll("\\\\","\\\\\\\\")
    //println(s"--- About to build mermaid $mermaid")
    val mermaidJs = MermaidJS(mermaidFixed,divBox,svgBox)
    //println(s"--------\nrunning mermaid code:\n--------\n$mermaidJs\n---------")
    scalajs.js.eval(mermaidJs)
    for st <- leafs do
      document.getElementById(mkStId(st)).asInstanceOf[html.Element].
        onclick = { (e: MouseEvent) => expandState(st) }
  } catch Widget.checkExceptions(errorBox,name)

  protected def mkStId(st:St) = (titleId,st).hashCode.toString
  private def mkAnchor(st:St,s:String) = s"<div id='${mkStId(st)}'>$s</div>"

  protected def mkMermaid: (String,Iterable[St]) =
    def fix(s: String): String =
      if s.startsWith("$$") then s.drop(2) else
      s" $s"
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")
        .replaceAll("\n", "<br>")
    def fixPlus(s:String): String =
      if s.trim.isEmpty then "+" else
      if s.startsWith("$$") then s.drop(2) else
         fix(s)
    def aux(next: Set[St], done: Set[St], leafs: Set[St]): (String,Set[St]) =
      next.headOption match
        // state found, but already processed.
        case Some(st) if done contains st => aux(next - st, done, leafs)
        // new state found
        case Some(st) =>
          val done2 = done + st
          var next2 = next - st
          // $$<button id='bbb' class='btNextTrans'>+</button>
          tree.get(st) match
            case Some(nexts) =>
              var res = s"\n  ${mkStId(st)}([\"${mkAnchor(st,fix(vS(st)))}\"]);"
              for (a, s2) <- nexts do
                next2 += s2
                res += s"\n  ${mkStId(s2)}([\"${fix(vS(s2))}\"]);\n  ${mkStId(st)} -->|\"${fix(vA(a))}\"| ${mkStId(s2)};"
              val (gr,lf) = aux(next2,done2,leafs)
              (res+gr, lf)
            case None =>
              var res = s"\n  ${mkStId(st)}([\"<button id='${mkStId(st)}' class='btLeaf'>${fixPlus(vS(st))}</button>\"]);"
              res += s"\n  style ${mkStId(st)} fill:#f87,stroke:#633,stroke-width:4px;"
              val (gr, lf) = aux(next2, done2, leafs)
              (res + gr, lf+st)
        // done: no more next states
        case None => ("",leafs)

    val (graph,leafs) = aux(Set(root),Set(),Set())
    (s"graph TD\n  style ${mkStId(root)} fill:#8f7,stroke:#363,stroke-width:4px;" + graph)
      -> leafs


}

