package caos.frontend.widgets

import WidgetInfo.Simulate
import caos.frontend.Documentation
import caos.sos.HasTaus
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}

/**
 * Created by   on 16/03/2021
 */

class SimulateText[Stx,Act,St](stx: () => Stx, simulate:Simulate[Stx,Act,St],
                               name:String, errorBox: OutputArea,
                               doc:Documentation)
  extends Widget[Unit](name,doc) {

  private var container:Block = _
  private var left:Block = _
  private var right:Block = _
  private var top:Block = _
  private val simBox = titleId+"Box" //name.replace(' ','_')+"Box"

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
    dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = { (_: MouseEvent) => if(!isVisible) initialise() }

    top = box
      .append("div")
      .style("width:100%;margin-bottom:10px;margin:5px 1px 5px 15px")

    val goBack: Unit = box.append("div").style("padding","5px 1px 5px 15px")
      .append("button")
      .textEl("undo")
      .on("click",() => undo())

    container = box.append("div")
      .style("display", "flex")
      .style("justify-content", "flex-start")
      .style("padding","5px 1px 5px 15px")

    left = container.append("div")
      .style("width:15%;min-width:9rem;border-right-style:solid;border-right-width:1px;border-right-color: #ddd;")

    right = container.append("div")
      .style("display:inline; width:100%;")
    right.append("div")
      .attr("id", simBox)
      .style("text-align", "left")
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
  } catch Widget.checkExceptions(errorBox,name)

  def initialiseWith(c:St, t:List[Act], s:List[St]):Unit = {
    lastStx = c
    traceActs = t
    traceStx = s
    updateEnabledActions(lastStx)
    updateSimulationSteps((None:: traceActs.map(Some(_))).zip(traceStx))
    //updateSimulation((None,lastChoreo)::Nil)
  }

  protected def undo():Unit =
    if (traceActs.size<=1) initialise()
    else initialiseWith(traceStx.init.last,traceActs.init,traceStx.init)

  protected def takeStep(a:Act,goesTo:St):Unit = try {
    lastStx = goesTo
    traceStx :+= goesTo
    //if (!a.isTau) traceActs :+=a
    traceActs :+=a // todo: extend SOS[A<:HasTaus,S]
    updateSimulationSteps((None:: traceActs.map(Some(_))).zip(traceStx))
    updateEnabledActions(goesTo)
  } catch Widget.checkExceptions(errorBox,name)

  def updateEnabledActions(c: St):Unit = {
    showTrace()
    showTerminal(c)
    showEnabled(c)
  }

  def showTrace():Unit = {
    top.text("")
    top.append("span").style("font-weight:bold;").textEl("Trace:")
      .append("span").style("font-weight:normal")
      .html(s""" ${traceActs.map(act=>
        s"<pre style=\"font-size: 1.2rem; width: fit-content; display: inline-grid; padding: 2.5px;\">${
          cleanHtml(simulate.lb(act))}</pre>").mkString(",")}""")
  }

  def showTerminal(from:St):Unit = {
    if simulate.sos.accepting(from)
    then top.append("p").append("span")
            .style("font-weight:bold;").textEl("- Terminal -")
  }

  def showEnabled(from:St):Unit = {
    left.html("")
    val enabled = simulate.sos.next(from) //.toSet
    //if (simulate.sos.canSkip(from)) //todo: add support
    //  enabled +:= ((Tau,Choreo.End))

    val ul = left.append("ul")
      .style("list-style-type:none;padding:0;margin:0;")//.attr("class", "list-group list-group-flush")
    ul.append("li")
      .append("span").style("font-weight:bold;").textEl("Enabled transitions:")

    for ((a,p)<-enabled.toList
          .sortWith((x,y)=>simulate.lb(x._1) < simulate.lb(y._1))) {
      val li = ul.append("li")
      val b = li.append("button")
        .attr("title",simulate.v(p).code)
        .attr("class","btNextTrans")
        .textEl(/*if (a.isTau) "terminate" else*/ simulate.lb(a)) // todo: handle taus
      b.on("click", () => { takeStep(a,p)})
    }
  }

  protected def updateSimulationSteps(sim: List[(Option[Act],St)]):Unit = {
    right.text("")
    right.html(sim.map(s => showStep(s)).mkString(""))
  }

  protected def showStep(step:(Option[Act],St)):String =
    s"""<div style="display:flex;justify-content:flex-start;padding:0px 1px 5px 15px;">
       |  ${showActStep(step._1)}
       |  ${showStStep(step._2)}
       |</div>""".stripMargin

  protected def showStStep(c:St):String =
    s"""<pre style="font-size: 1.2rem; width: fit-content; display: inline-grid; padding: 2.5px; overflow:visible;">
       |${cleanHtml(simulate.v(c).code)}
       |</pre>""".stripMargin
//    s"""<div style="display:inline;width:100%;text-align:left;">
//       |${simulate.v(c).code}
//       |</div>""".stripMargin

  private def cleanHtml(str: String): String =
    str.replaceAll("<","&#60;")
      .replaceAll(">","&#62;")

  protected def showActStep(a:Option[Act]):String = {
    s"""<div style="text-align:left;width:15%;font-weight:bold;">
       |  ${if (a.isDefined)  s"""${cleanHtml(a.get.toString)} &#8594;""" else "&#8594;"}
       |</div>""".stripMargin
  }

  //protected def htmlChoreo(c:Choreo):String =
  //  c.toString.replace("->","&#8594;")

}

