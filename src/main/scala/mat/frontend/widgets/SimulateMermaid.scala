package mat.frontend.widgets

import mat.frontend.Configurator.Simulate
import mat.frontend.widgets.{Box, MermaidJS, OutputArea}
import mat.sos.HasTaus
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}

/**
 * Created by guillecledou on 16/03/2021
 */

class SimulateMermaid[Stx,Act,St](stx: () => Stx, simulate:Simulate[Stx,Act,St], name:String, errorBox: OutputArea)
  extends Box[Unit](name,Nil) {

  private var container:Block = _
  private var left:Block = _
  private var right:Block = _
  private var top:Block = _
  //private val simBox = name.replace(' ','_')+"Box"
  protected val svgBox = name.replace(' ','_') + "Svg"
  protected val divBox = name.replace(' ','_') + "Box"

  override def get: Unit = ()

  protected var traceActs:List[Act] = List()
  protected var lastStx:St = _
  protected var traceStx:List[St] = List()


  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean=false): Unit = {
    val box = panelBox(div, visible, buttons = List(
      Right("refresh") -> (() =>
        update(), "Simulate next actions of current program")
    ))
    dom.document.getElementById(name).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = { (e: MouseEvent) => if(!isVisible) initialise() }

    top = box
      .append("div")
      .style("width:100%;margin-bottom:10px;margin:5px 1px 5px 15px")

    val goBack = box.append("div").style("padding","5px 1px 5px 15px")
      .append("button")
      .textEl("undo")
      .on("click",() => undo())

    container = box.append("div")
      .style("display", "flex")
      .style("justify-content", "flex-start")
      .style("padding","5px 1px 5px 15px")

    left = container.append("div")
      .style("width:15%; border-right-style:solid;border-right-width:1px;border-right-color: #ddd;")

    right = container.append("div")
      .style("display:inline; width:100%;")
    right.append("div")
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
    val c = simulate.pre(stx())//DSL.pomset(choreography)
    initialiseWith(c,Nil,c::Nil)
  } catch Box.checkExceptions(errorBox)

  def initialiseWith(c:St, t:List[Act], s:List[St]):Unit = {
    lastStx = c
    traceActs = t
    traceStx = s
    updateEnabledActions(lastStx)
    updateSimulationSteps((None::(traceActs.map(Some(_)))).zip(traceStx))
    //updateSimulation((None,lastChoreo)::Nil)
  }

  protected def undo():Unit =
    if (traceActs.size<=1) initialise()
    else initialiseWith(traceStx.init.last,traceActs.init,traceStx.init)

  protected def takeStep(a:Act,goesTo:St):Unit = {
    lastStx = goesTo
    traceStx :+= goesTo
    //if (!a.isTau) traceActs :+=a
    traceActs :+=a // todo: extend SOS[A<:HasTaus,S]
    updateSimulationSteps((None::(traceActs.map(Some(_)))).zip(traceStx))
    updateEnabledActions(goesTo)
  }

  def updateEnabledActions(c: St):Unit = {
    showTrace()
    showEnabled(c)
  }

  def showTrace():Unit = {
    top.text("")
    top.append("span").style("font-weight:bold;").textEl("Trace:")
      .append("span").style("font-weight:normal")
      .text(s""" ${traceActs.mkString(", ")}""")
  }

  def showEnabled(from:St):Unit = {
    left.html("")
    var enabled = simulate.sos.next(from)//.toSet
    //if (simulate.sos.canSkip(from)) //todo: add support
    //  enabled +:= ((Tau,Choreo.End))

    val ul = left.append("ul")
      .style("list-style-type:none;padding:0;margin:0;")//.attr("class", "list-group list-group-flush")
    ul.append("li")
      .append("span").style("font-weight:bold;").textEl("Enabled transitions:")

    for ((a,p)<-enabled) {
      val li = ul.append("li")
      val b = li.append("button").attr("title",p.toString)
        .textEl(/*if (a.isTau) "terminate" else*/ a.toString) // todo: handle taus
      b.on("click", () => { takeStep(a,p)})
    }
  }

  protected def updateSimulationSteps(sim: List[(Option[Act],St)]):Unit = {
    if sim.nonEmpty then showSt(sim.last._2)
  }


  protected def showSt(st:St):Unit = try {
    val mermaid = simulate.v(st).code
    val mermaidJs = MermaidJS(mermaid,divBox,svgBox)
    scalajs.js.eval(mermaidJs)
  } catch Box.checkExceptions(errorBox)

}

