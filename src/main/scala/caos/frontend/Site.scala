package caos.frontend

import widgets.{CodeWidget, DomElem, DomNode, ExampleWidget, Invisible, OutputArea, SimulateMermaid, SimulateText, Tabs, Utils, VisualiseCode, VisualiseMermaid, VisualiseOptMermaid, VisualiseText, Widget, WidgetInfo}
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

    // find the main example (from URL of first in the list)
    val urlQuery = document.URL.split('?').drop(1).mkString("?")
      .replaceAll("%2F", "/") .replaceAll("%3C", "<")
      .replaceAll("%26", "&") .replaceAll("%3E", ">")
      .replaceAll("%20", " ") .replaceAll("%23", "#")
      .replaceAll("%24", "$") .replaceAll("%7B", "{")
      .replaceAll("%2B", "+") .replaceAll("%7D", "}")
      .replaceAll("%2C", ",") .replaceAll("%7C", "|")
      .replaceAll("%3A", ":") .replaceAll("%5E", "^")
      .replaceAll("%3B", ";") .replaceAll("%7E", "~")
      .replaceAll("%3F", "?") .replaceAll("%5B", "[")
      .replaceAll("%40", "@") .replaceAll("%5D", "]")
      .replaceAll("%22", "\"").replaceAll("%60", "`")
      .replaceAll("%28", "(") .replaceAll("%29", ")")

    val mainExample = config.examples.find(_.name == urlQuery) match
      case None =>
        if urlQuery.nonEmpty
        then Some(Configurator.Example(urlQuery,"Custom",""))
        else config.examples.headOption
      case ex => ex

    errorArea = new OutputArea
    descriptionArea = new OutputArea
    val code = mkCodeBox(config,mainExample)

    code.init(leftColumn,true)
    errorArea.init(leftColumn)

    val title = document.getElementById("title")
    val toolTitle = document.getElementById("tool-title")
    title.textContent = config.name
    toolTitle.textContent = config.name


    //val ex = (for ((n,e) <- config.examples) yield n::e::n::Nil).toSeq
    val examples = new ExampleWidget("Examples",config,globalReload(),code,Some(descriptionArea))

    // place examples and information area
    descriptionArea.init(leftColumn) // before the examples
    examples.init(leftColumn,true)

    // build and place all widgets
    // small widgets are deprecated - this makes it work with older versions.
    val widgets = (for (name,wi) <-config.smallWidgets yield (name,wi.moveTo(1))) ++ config.widgets
    val boxes = for wc <- widgets yield
      // build widget w
      val w = mkWidget(wc, () => code.get,
        () => examples.get.map(kv => kv._1 -> config.parser(kv._2)),
        errorArea,
        config.documentation
      )
      // place widget in the document
      w.init(if wc._2.location == 0 then rightColumn else leftColumn, wc._2.expanded)
      w

//    // build main boxes
//    val boxes = config.widgets.map(w => mkWidget(w, ()=>code.get,
//        ()=>examples.get.map(kv=>kv._1->config.parser(kv._2)), errorArea))
//    boxes.foreach(b=>b.init(rightColumn,false))

//    // build small boxes
//    val smallBoxes = config.smallWidgets.map(w => mkWidget(w, () => code.get,
//      () => examples.get.map(kv => kv._1 -> config.parser(kv._2)), errorArea))
//    smallBoxes.foreach(b => b.init(rightColumn, false))


    mainExample match
      case Some(ex) => if (ex.description.nonEmpty) descriptionArea.setValue(ex.description)
      case _ =>

    toReload = (List(code)++boxes).map(b => ()=>b.update())

    globalReload()

  /**
   * Make widget box
   * @param w widget info
   * @param get function to get program
   * @param out output box to output errors
   * @tparam Stx Type of the program to process
   * @return a box
   */
  protected def mkWidget[Stx](w: (String,WidgetInfo[Stx]), get:()=>Stx,
                              getAll:()=>Seq[(String,Stx)], out:OutputArea,
                              doc: Documentation
                             ): Widget[Unit] =
    try w._2 match {
      case Visualize(view,Mermaid,pre) => new VisualiseMermaid(()=>view(pre(get())),w._1,out,doc)
      case Visualize(view,Text,pre) => new VisualiseText(()=>view(pre(get())),w._1,out,doc)
      case Visualize(view,Code(lang),pre) => new VisualiseCode(()=>view(pre(get())),w._1,lang,out,doc)
      case VisualizeAll(v,Mermaid,pre) => new VisualiseMermaid(()=>v(getAll().map(kv=>kv._1->pre(kv._2))),w._1,out,doc)
      case VisualizeAll(v,Text,pre) => new VisualiseText(()=>v(getAll().map(kv=>kv._1->pre(kv._2))),w._1,out,doc)
      case Visualize(_,Html,_) | VisualizeAll(_,Html,_) =>
        out.setValue("HTML visualiser not supported")
        sys.error("HTML visualiser not supported")
      case VisualizeTab(views,Text,titles,pre) =>
        new Tabs(()=>views(pre(get())),w._1,()=>titles(pre(get())),"",out,doc) // no language produces text boxes
      case VisualizeTab(views, Code(lang), titles, pre) =>
        new Tabs(() => views(pre(get())), w._1, () => titles(pre(get())), lang, out,doc)
      case VisualizeOpt(view,t, pre) => t match {
        case Mermaid => new VisualiseOptMermaid(()=>view(pre(get())),w._1,out,doc)
        case _ => throw new RuntimeException("case not covered...")
      }
      case sim@Simulate(_, _, _, t, _) => t match { // view(pre(get())) match {
        case Text => new SimulateText(get,sim, w._1, out,doc)
        case Mermaid => new SimulateMermaid(get,sim,w._1,out,doc)
        case _ => throw new RuntimeException(s"case not covered when compiling widget '${w._1}': $sim")
      }
      case Explore(init,sos,vS,vA) => new widgets.Explore(()=>init(get()),sos,vS,vA,w._1,out,doc)
      case Analyse(a) =>
        new Invisible[Stx,Unit](get, stx =>  (a(stx),Nil,()),w._1)
      case _ => throw new RuntimeException(s"case not covered when compiling widget '${w._1}': ${w._2}")
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

    val overlay = contentDiv.append("div")
      .attr("class","overlay")
      .attr("id","CAOSOverlay")
    val popup = contentDiv.append("div")
      .attr("class","popup")
      .attr("id","CAOSPopupWrp")
    val closePop = popup.append("div")
      .attr("class", "closePopup")
    popup.append("div")
      .attr("id","CAOSPopup")
    closePop.on("click",()=>{
      dom.document.getElementById("CAOSOverlay").setAttribute("style","display:none;")
      dom.document.getElementById("CAOSPopupWrp").setAttribute("style","display:none;")
    })
    overlay.on("click", () => {
      dom.document.getElementById("CAOSOverlay").setAttribute("style", "display:none;")
      dom.document.getElementById("CAOSPopupWrp").setAttribute("style", "display:none;")
    })
    closePop.append("div")
      .attr("id","CAOSPopupTitle")
      .attr("style","display:inline-block;font-weight: bold;")
//      .html("title")
    closePop.append("div")
      .attr("style","float:right;")
      .html("&#10006;")

    if lastConfig.isDefined && lastConfig.get.footer!=""
    then contentDiv.append("div")
      .style("width: 100%;text-align: center; display: inline-block;")
      .html(s"&nbsp;<br><p style=\"margin: 0px 30px 10px;\">${lastConfig.get.footer}</p>")

    //<div style="width: 100%;text-align: center; display: inline-block;">
    //    &nbsp;<br>
    //    <p style="margin: 0px 30px 10px;">Source code at: <a href="https://github.com/arcalab/choreo/tree/ceta" target="#">https://github.com/arcalab/choreo/tree/ceta</a>.
    //    This is a companion tool for a paper under revision.
    //    </p>
    //</div>

    Utils.resizeCols


  protected def globalReload(): Unit =
    errorArea.clear()
    toReload.foreach(f=>f())

  protected def mkCodeBox[A](config:Configurator[A],
                             ex:Option[Configurator.Example]):CodeWidget[A] =
    new CodeWidget[A](config.languageName,Nil) {

      protected var input: String = ex match
        case Some(e) => e.example
        case _ => ""

      override protected val boxId: String = config.name + "Box"

      override protected val buttons: List[(Either[String, String], (() => Unit, String))] =
        List(Right("refresh") -> (() => reload(), s"Load the ${config.languageName} program (shift-enter)"))
        :::
          Widget.mkHelper(config.languageName,config.documentation).toList

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
      val resultAsString = Utils.unfix(reader.result.toString)
      //println("Loaded?")
      lastConfig match {
        case Some(c:Configurator[A] @unchecked)  =>
          val c2 = new Configurator[A] {
            override val parser = c.parser
            override val name: String = c.name
            override val languageName: String = c.languageName
            override val widgets = c.widgets
            override val examples: Iterable[Configurator.Example] =
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
