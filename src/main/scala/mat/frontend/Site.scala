package mat.frontend

import mat.frontend.Configurator.{Visualize, Widget}
import mat.frontend.widgets.{Box, CodeBox, DomElem, DomNode, OutputArea, VisualiseMermaid}
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

    // todo: iterate trough all widgets
    val x = config.widgets.head
    val y = mkBox(x,()=>code.get,errorArea)
    y.init(rightColumn,true)
    toReload = List(y.update)
    //globalReload()

  private def mkBox[Stx](w: (Widget[Stx], String),get:()=>Stx,out:OutputArea): Box[Unit] =
    w._1 match {
        //todo: nicer way to achieve this type check?
      case vis@Visualize(view,pre):Visualize[Stx,_] => view(pre(get())) match {
        case v:Mermaid => new VisualiseMermaid(()=>view(pre(get())),w._2,out)//(view) compose (pre ) compose get,w._2,out)
        case _: Text => sys.error("Text visualiser not supported")
        case _: Html => sys.error("HTML visualiser not supported")
      }
      case _ => throw new RuntimeException("case not covered...")
    }


  def initialiseContainers():Unit =
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

  private def globalReload(): Unit = toReload.foreach(f=>f())


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

  //@JSExportTopLevel("mat_frontend_Site_main")
  //def main():Unit = //(args: Array[String]): Unit =
  //  println("Hello world in the console!")
  //
  //  val contentDiv = DomNode.select("contentWrap").append("div")
  //    .attr("class", "content")
  //
  //  contentDiv.text("I'm in the browser!")


//package mat.frontend.widgets
//
//import mat.frontend.Arcatools.{Visualize, Widget}
//import mat.frontend.Arcatools
//import mat.view.View
//import org.scalajs.dom.html
//
//import scala.scalajs.js.annotation.JSExportTopLevel
//
//object Main:
//
//  val config: Arcatools[_] = ChoreoAC
//
////  var errorArea: OutputArea = _
////  var code: CodeBox = _
//
//  @JSExportTopLevel("choreo_frontend_widgets_Main_main")
//  def main(content: html.Div): Unit =
//    // Creating outside containers:
//    val contentDiv = DomNode.select(content).append("div")
//      .attr("class", "content")
//
//    val rowDiv = contentDiv.append("div")
//      //      .attr("class", "row")
//      .attr("id", "mytable")
//
//    val leftColumn = rowDiv.append("div")
//      //      .attr("class", "col-sm-4")
//      .attr("id", "leftbar")
//      .attr("class", "leftside")
//
//    leftColumn.append("div")
//      .attr("id", "dragbar")
//      .attr("class", "middlebar")
//
//    val rightColumn = rowDiv.append("div")
//      //      .attr("class", "col-sm-8")
//      .attr("id", "rightbar")
//      .attr("class", "rightside")
//
//    val errorArea = new OutputArea
//
//    val code = new CodeBox[config.T](config.name,Nil) {
//      protected var input: String = "<program>"
//      override protected val boxId: String = config.name+"Box"
//      override protected val buttons: List[(Either[String, String], (() => Unit, String))] =
//        List(
//          Right("refresh") -> (() => reload(), s"Load the ${config.name} program (shift-enter)")
//        )
//
//      override def get: config.T = config.parser(input)
//
//      override def reload(): Unit =
//        update()
//        errorArea.clear()
//        globalReload()
//
//      override protected val codemirror: String = config.name
//    }
//
//    val x = config.widgets.head
//    val y = mkBox(x,()=>code.get,errorArea)
//
//
//  private def mkBox[Stx](w: (Widget[Stx], String),get:()=>Stx,out:OutputArea): Box[Unit] =
//    w._1 match {
//      case Visualize(view,pre):Visualize[Stx,_] => view(pre(get())) match {
//        case v: choreo.view.Mermaid => new VisualiseMermaid(()=>v.code,config.name,out)
//        case _: choreo.view.Text => sys.error("Text visualiser not supported")
//        case _: choreo.view.Html => sys.error("HTML visualiser not supported")
//      }
//      case _ => throw new RuntimeException("case not covered...")
//    }
//
//
//  private def globalReload(): Unit = {}
//
//
