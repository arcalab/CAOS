package caos.frontend

import caos.common.Example
import widgets.{CodeWidget, DomElem, DomNode, ExampleWidget, Invisible, OutputArea, SimulateMermaid, SimulateText, Tabs, Utils, VisualiseMermaid, VisualiseOptMermaid, VisualiseText, VisualiseWarning, Widget, WidgetInfo}
import WidgetInfo.*
import caos.view.*
import caos.view.OptionView.*
import org.scalajs.dom
import org.scalajs.dom.{FileReader, document, html}

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Site:

  var leftColumn:DomElem = _
  var rightColumn:DomElem = _
  var errorArea:OutputArea = _
  var descriptionArea: OutputArea = _
  var toReload:List[()=>Unit] = _
  var lastConfig: Option[Configurator[_]] = None

  def initSite[A](config:Configurator[A]):Unit =
    lastConfig = Some(config)
    initialiseContainers()

    errorArea = new OutputArea
    descriptionArea = new OutputArea
    val code = mkCodeBox(config)

    code.init(leftColumn,true)
    errorArea.init(leftColumn)

    val title = document.getElementById("title")
    val toolTitle = document.getElementById("tool-title")
    title.textContent = config.name
    toolTitle.textContent = config.name


    //val ex = (for ((n,e) <- config.examples) yield n::e::n::Nil).toSeq
    val examples = new ExampleWidget("Examples",config.examples,globalReload(),code,Some(descriptionArea))

    val boxes = config.widgets.map(w => mkWidget(w, ()=>code.get,
        ()=>examples.get.map(kv=>kv._1->config.parser(kv._2)), errorArea))
    boxes.foreach(b=>b.init(rightColumn,false))

    examples.init(leftColumn,true)
    descriptionArea.init(leftColumn) // after the examples

    val smallBoxes = config.smallWidgets.map(w => mkWidget(w,()=>code.get,
        ()=>examples.get.map(kv=>kv._1->config.parser(kv._2)),errorArea))
    smallBoxes.foreach(b=>b.init(leftColumn,false))


    config.examples.headOption match
      case Some(ex) => if (ex.description.nonEmpty) descriptionArea.setValue(ex.description)
      case _ =>

    toReload = (List(code)++boxes++smallBoxes).map(b => ()=>b.update())

  /**
   * Make widget box
   * @param w widget info
   * @param get function to get program
   * @param out output box to output errors
   * @tparam Stx Type of the program to process
   * @return a box
   */
  protected def mkWidget[Stx](w: (String,WidgetInfo[Stx]), get:()=>Stx, getAll:()=>Seq[(String,Stx)], out:OutputArea): Widget[Unit] =
    try w._2 match {
        //todo: nicer way to achieve this type check?
//      case Visualize(view:Mermaid,pre) => new VisualiseMermaid(()=>view(pre(get())),w._1,out)
//      case Visualize(view:Text,pre) => new VisualiseText(()=>view(pre(get())),w._1,out)
//      case Visualize(view:Html,_) =>
//        out.setValue("HTML visualiser not supported")
//        sys.error("HTML visualiser not supported")

      case Visualize(view,Mermaid,pre) => new VisualiseMermaid(()=>view(pre(get())),w._1,out)
      case Visualize(view,Text,pre) => new VisualiseText(()=>view(pre(get())),w._1,out)
      case VisualizeAll(v,Mermaid,pre) => new VisualiseMermaid(()=>v(getAll().map(kv=>kv._1->pre(kv._2))),w._1,out)
      case VisualizeAll(v,Text,pre) => new VisualiseText(()=>v(getAll().map(kv=>kv._1->pre(kv._2))),w._1,out)
      case Visualize(_,Html,_) | VisualizeAll(_,Html,_) =>
        out.setValue("HTML visualiser not supported")
        sys.error("HTML visualiser not supported")
      case VisualizeTab(views,Text,titles,pre) =>
        new Tabs(()=>views(pre(get())),w._1,()=>titles(pre(get())),out)
      case VisualizeWarning(view, Text, pre) => 
        VisualiseWarning(()=>view(pre(get())),w._1,out)
//      case Visualize(view, pre): Visualize[Stx, _] => view(pre(get())) match {
//        case v:Mermaid => new VisualiseMermaid(()=>view(pre(get())),w._1,out)
//        case _: Text => new VisualiseText(()=>view(pre(get())),w._1,out) //sys.error("Text visualiser not supported")
//        case _: Html => sys.error("HTML visualiser not supported")

      case VisualizeOpt(view,t, pre): VisualizeOpt[Stx, _] => t match {
        case Mermaid => new VisualiseOptMermaid(()=>view(pre(get())),w._1,out)
        case _ => throw new RuntimeException("case not covered...")
      }
      case sim@Simulate(_, _, t, _): Simulate[Stx, _, _] => t match { // view(pre(get())) match {
        case Text => new SimulateText(get,sim, w._1, out)
        case Mermaid => new SimulateMermaid(get,sim,w._1,out)
        case _ => throw new RuntimeException("case not covered...")
      }
      case Analyse(a) =>
        new Invisible[Stx,Unit](get, stx =>  (a(stx),Nil,()))
      case _ => throw new RuntimeException("case not covered...")
    } catch {
      case e: Throwable =>
        out.error(e.getMessage)
        throw e
    }

  protected def cleanContainers():Unit = {
    //    val contentDiv = DomNode.select("contentWrap")
    //    contentDiv.deleteChildren
    val d = document.getElementById("contentWrap")
    val d_nested = document.getElementById("content")
    d.removeChild(d_nested)
  }


  protected def initialiseContainers():Unit =
    val contentDiv = DomNode.select("contentWrap").append("div")
      .attr("class", "content")
      .attr("id", "content")

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
//      .style("margin-left","24%")

    rightColumn = rowDiv.append("div")
      //      .attr("class", "col-sm-8")
      .attr("id", "rightbar")
      .attr("class", "rightside")

    Utils.resizeCols


  protected def globalReload(): Unit =
    errorArea.clear()
    toReload.foreach(f=>f())

  protected def mkCodeBox[A](config:Configurator[A]):CodeWidget[A] =
    new CodeWidget[A](config.languageName,Nil) {

      protected var input: String = config.examples.headOption match
        case Some(ex) => ex.example
        case _ => ""

      override protected val boxId: String = config.name + "Box"

      override protected val buttons: List[(Either[String, String], (() => Unit, String))] =
        List(
          Right("refresh") -> (() => reload(), s"Load the ${config.name} program (shift-enter)")
        )

      override def get: A = config.parser(input)

      override protected val codemirror: String = "caos" //config.name.toLowerCase()

      override def reload(): Unit =
        descriptionArea.clear()
        update()
        //out.clear() // now already in globalReload()
        globalReload()
    }

//  @JSExportTopLevel("loadedFile")
//  def loadedFile(ev: dom.UIEvent): Unit = {
//    println("File Loaded Successfully...");
//    var fileString = ev.target; // result
//    println(fileString);
////    $$("#op").text(fileString);
////    appendPar(document.body, "You clicked the button!")
//  }

  /** This method loads the examples from a local file. */
  @JSExportTopLevel("getFileAsText")
  def getFileAsText[A](ev: dom.File): Unit = {
    val reader = new dom.FileReader()
    reader.readAsText(ev)
    reader.onload = _ => {
      val resultAsString = reader.result.toString
      //println("Loaded?")
      lastConfig match {
        case Some(c:Configurator[A] @unchecked)  =>
          val c2 = new Configurator[A] {
            override val parser = c.parser
            override val name: String = c.name
            override val languageName: String = c.languageName
            override val widgets = c.widgets
            override val examples: Iterable[Example] =
              ExampleWidget.txtToExamples(resultAsString)
          }
          //println("init site again")
          cleanContainers()
          initSite(c2)
        case _ =>
          //println("no config...")
      }
      //println(resultAsString)
    }
  }
