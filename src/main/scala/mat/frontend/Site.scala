package mat.frontend

import mat.frontend.Configurator.{Simulate, Visualize, Widget}
import mat.frontend.widgets.{Box, CodeBox, DomElem, DomNode, OutputArea, VisualiseMermaid, SimulateText, SimulateMermaid}
import mat.view._
import org.scalajs.dom.{document, html}

import scala.scalajs.js
import scala.scalajs.js.annotation._

object Site:

  var leftColumn:DomElem = _
  var rightColumn:DomElem = _
  var errorArea:OutputArea = _
  var descriptionArea: OutputArea = _
  var toReload:List[()=>Unit] = _

  def initSite[A](config:Configurator[A]):Unit =
    initialiseContainers()

    errorArea = new OutputArea
    descriptionArea = new OutputArea
    val code = mkCodeBox(config,errorArea)

    code.init(leftColumn,true)
    errorArea.init(leftColumn)
    descriptionArea.init(leftColumn)

    val title = document.getElementById("title")
    title.textContent = config.name

    val boxes = config.widgets.take(5).map(w => mkBox(w,()=>code.get,errorArea))
    boxes.foreach(b=>b.init(rightColumn,true))
    toReload = boxes.map(b => ()=>b.update()).toList


  protected def mkBox[Stx](w: (Widget[Stx], String),get:()=>Stx,out:OutputArea): Box[Unit] =
    w._1 match {
        //todo: nicer way to achieve this type check?
      case Visualize(view,pre):Visualize[Stx,_] => view(pre(get())) match {
        case v:Mermaid => new VisualiseMermaid(()=>view(pre(get())),w._2,out)
        case _: Text => sys.error("Text visualiser not supported")
        case _: Html => sys.error("HTML visualiser not supported")
      }
      case sim@Simulate(sos, view, pre):Simulate[Stx,_,_] => view(pre(get())) match {
        case v:Text => new SimulateText(get,sim, w._2, out)
        case v:Mermaid => new SimulateMermaid(get,sim,w._2,out)
        case _ => throw new RuntimeException("case not covered...")
      }
      case _ => throw new RuntimeException("case not covered...")
    }


  protected def initialiseContainers():Unit =
    val contentDiv = DomNode.select("contentWrap").append("div")
      .attr("class", "content")

    val rowDiv = contentDiv.append("div")
      //      .attr("class", "row")
      .attr("id", "mytable")

    leftColumn = rowDiv.append("div")
      //      .attr("class", "col-sm-4")
      .attr("id", "leftbar")
      .attr("class", "leftside")

    leftColumn.append("div")
      .attr("id", "dragbar")
      .attr("class", "middlebar")

    rightColumn = rowDiv.append("div")
      //      .attr("class", "col-sm-8")
      .attr("id", "rightbar")
      .attr("class", "rightside")

  protected def globalReload(): Unit = toReload.foreach(f=>f())

  protected def mkCodeBox[A](config:Configurator[A],out:OutputArea):CodeBox[A] =
    new CodeBox[config.T](config.name,Nil) {

      protected var input: String = ""

      override protected val boxId: String = config.name + "Box"

      override protected val buttons: List[(Either[String, String], (() => Unit, String))] =
        List(
          Right("refresh") -> (() => reload(), s"Load the ${config.name} program (shift-enter)")
        )

      override def get: config.T = config.parser(input)

      override protected val codemirror: String = config.name

      override def reload(): Unit =
        update()
        out.clear()
        globalReload()
    }
