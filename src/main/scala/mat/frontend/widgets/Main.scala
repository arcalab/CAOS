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
//    val y = createBox(x,()=>code.get,errorArea)
//
//
//  private def createBox[Stx](w: (Widget[Stx], String),get:()=>Stx,out:OutputArea): Box[Unit] =
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
