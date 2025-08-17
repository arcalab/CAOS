package caos.frontend

import widgets.{CodeWidget, DomNode, ExampleWidget, Invisible, OutputArea, SettingWidget, SimulateMermaid, SimulateText, Tabs, Utils, VisualiseCode, VisualiseMermaid, VisualiseOptMermaid, VisualiseText, Widget, WidgetInfo}
import WidgetInfo.*
import caos.view.*
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Site {
  /* currently using '''state''' as the sole 'global var' */
  private var state: SiteState = SiteState()

  /** simple getter for '''Setting''' under the current '''state'''*/
  /* can be called upon by the final user */
  def getSetting: Setting = {
    state.getSettingWidget.get
  }

  /** simple setter for '''Setting''' under the current '''state''' */
  /* can be called upon by the final user */
  def setSetting(setting: Setting): Unit = {
    state.getSettingWidget.set(setting)
    state.getSettingWidget.update()
    globalReload()
  }

  def initSite[A](config: Configurator[A]): Unit = {
    dom.document.getElementById("title").textContent = config.name
    dom.document.getElementById("tool-title").textContent = config.name

    state = state.withLastConfig(config)
    initialiseContainers()

    state = state
      .withErrorArea(new OutputArea)
      .withDescriptionArea(new OutputArea)

    val mainExample = resolveMainExample(config, parseURLQuery)
    val codeWidget  = mkCodeBox(config, Some(mainExample))
    state = state.withCodeWidget(codeWidget)
    codeWidget.init(state.getLeftColumn, true)

    state.getErrorArea.init(state.getLeftColumn)

    val settingWidget = mkSettingBox(config)
    state = state.withSettingWidget(settingWidget)
    settingWidget.init(state.getLeftColumn, true)

    val examplesWidget = new ExampleWidget("Examples", config, globalReload(), codeWidget, Some(state.getDescriptionArea), settingWidget)
    state = state.withExamplesWidget(examplesWidget)

    state.getDescriptionArea.init(state.getLeftColumn)
    state.getExamplesWidget.init(state.getLeftColumn, true)

    mainExample match {
      case example if example.description.nonEmpty =>
        state.getDescriptionArea.setValue(example.description)
        setSetting(example.setting.get)
      case _ =>
    }
  }

  private def parseURLQuery: String = {
    dom.document.URL.split('?').drop(1).mkString("?")
      .replaceAll("%2F", "/").replaceAll("%3C", "<")
      .replaceAll("%26", "&").replaceAll("%3E", ">")
      .replaceAll("%20", " ").replaceAll("%23", "#")
      .replaceAll("%24", "$").replaceAll("%7B", "{")
      .replaceAll("%2B", "+").replaceAll("%7D", "}")
      .replaceAll("%2C", ",").replaceAll("%7C", "|")
      .replaceAll("%3A", ":").replaceAll("%5E", "^")
      .replaceAll("%3B", ";").replaceAll("%7E", "~")
      .replaceAll("%3F", "?").replaceAll("%5B", "[")
      .replaceAll("%40", "@").replaceAll("%5D", "]")
      .replaceAll("%22", "\"").replaceAll("%60", "`")
      .replaceAll("%28", "(").replaceAll("%29", ")")
  }

  private def resolveMainExample[A](config: Configurator[A], urlQuery: String): Configurator.Example = {
    val example = config.examples.find(_.name == urlQuery)
      .getOrElse(if urlQuery.nonEmpty then Configurator.Example(urlQuery, "Custom", "") else config.examples.head)
    example
  }

  private def renderWidgets(): Unit = {
    val config = state.getLastConfig
    val code   = state.getCodeWidget

    val boxes = for wc <- config.widgets if wc._2.isDefined yield
      val w = mkWidget(
        wc,
        () => code.get.asInstanceOf[config.StxType],
        () => state.getExamplesWidget.get.map(kv => kv._1 -> config.parser(kv._2)),
        state.getErrorArea,
        config.documentation
      )
      // place widget in the document
      w.init(if wc._2.get.location == 0 then state.getRightColumn else state.getLeftColumn, wc._2.get.expanded)
      w

    state = state.withToReload(boxes.toList.map(b => () => b.update()))
  }

  private def initialiseContainers(): Unit = {
    val contentDiv = DomNode.select("contentWrap").append("div")
      .attr("class", "content")
      .attr("id", "content")

    val rowDiv = contentDiv.append("div")
      .attr("id", "mytable")

    val leftColumn = rowDiv.append("div")
      .attr("id", "leftbar")
      .attr("class", "leftside")

    state = state.withLeftColumn(leftColumn)

    state.getLeftColumn.append("div")
      .attr("id", "dragbar")
      .attr("class", "middlebar")

    val rightColumn = rowDiv.append("div")
      .attr("id", "rightbar")
      .attr("class", "rightside")

    state = state.withRightColumn(rightColumn)

    val overlay = contentDiv.append("div")
      .attr("class", "overlay")
      .attr("id", "CAOSOverlay")

    val popup = contentDiv.append("div")
      .attr("class", "popup")
      .attr("id", "CAOSPopupWrp")

    val closePop = popup.append("div")
      .attr("class", "closePopup")

    popup.append("div")
      .attr("id", "CAOSPopup")

    closePop.on("click", () => {
      dom.document.getElementById("CAOSOverlay").setAttribute("style", "display:none;")
      dom.document.getElementById("CAOSPopupWrp").setAttribute("style", "display:none;")
    })

    overlay.on("click", () => {
      dom.document.getElementById("CAOSOverlay").setAttribute("style", "display:none;")
      dom.document.getElementById("CAOSPopupWrp").setAttribute("style", "display:none;")
    })

    closePop.append("div")
      .attr("id", "CAOSPopupTitle")
      .attr("style", "display:inline-block;font-weight: bold;")

    closePop.append("div")
      .attr("style", "float:right;")
      .html("&#10006;")

    if state.getLastConfig.footer != "" then contentDiv.append("div")
      .style("width: 100%;text-align: center; display: inline-block;")
      .html(s"&nbsp;<br><p style=\"margin: 0px 30px 10px;\">${state.getLastConfig.footer}</p>")

    Utils.resizeCols
  }

  private def globalReload(): Unit = {
    state.getErrorArea.clear()
    dom.document.getElementById("rightbar").innerHTML = ""
    renderWidgets()
    state.getCodeWidget.update()
    state.getSettingWidget.update()
    state.getToReload.foreach(f => f())
  }

  private def mkCodeBox[A](config: Configurator[A], ex: Option[Configurator.Example]): CodeWidget[A] = {
    new CodeWidget[A](config.languageName, Nil) {
      protected var input: String = ex match {
        case Some(e) => e.example
        case _       => ""
      }

      override protected val boxId: String = config.name + "Box"

      override protected val buttons: List[(Either[String, String], (() => Unit, String))] = {
        List(
          Right("refresh") -> (
            () => reload(),
            s"Load the ${config.languageName} program (shift-enter)"
          ),
          Right("download") -> (
            () => Utils.downloadTxt(ExampleWidget.examplesToTxt(state.getExamplesWidget.getCurrentExample), "example.txt"),
            "Download Current Example"
          ),
        ) ::: Widget.mkHelper(config.languageName, config.documentation).toList
      }

      override def get: A = config.parser(input)

      override protected val codemirror: String = "caos" //config.name.toLowerCase()

      override def reload(): Unit = {
        state.getDescriptionArea.clear()
        globalReload()
        update()
      }
    }
  }

  private def mkSettingBox[A](config: Configurator[A]): SettingWidget[A] = {
    new SettingWidget[A]("Settings", Documentation(), config) {
      override protected val buttons: List[(Either[String, String], (() => Unit, String))] = {
        List(
          Right("refresh") -> (
            () => reload(),
            s"Load settings"
          )
        ) ::: Widget.mkHelper("settingBox", config.documentation).toList
      }

      override def reload(): Unit = {
        // settingWidget reload's should not clear description
        globalReload()
        update()
      }
    }
  }

  /**
   * Make widget box
   *
   * @param w   widget info
   * @param get function to get program
   * @param out output box to output errors
   * @tparam Stx Type of the program to process
   * @return a box
   */
  private def mkWidget[Stx](w: (String, Option[WidgetInfo[Stx]])
                            , get: () => Stx
                            , getAll: () => Seq[(String, Stx)]
                            , out: OutputArea
                            , doc: Documentation
                           ): Widget[Unit] = {
    try w._2.get match {
      case Visualize(view, Mermaid, pre) => new VisualiseMermaid(() => view(pre(get())), w._1, out, doc)
      case Visualize(view, Text, pre) => new VisualiseText(() => view(pre(get())), w._1, out, doc)
      case Visualize(view, Code(lang), pre) => new VisualiseCode(() => view(pre(get())), w._1, lang, out, doc)
      case VisualizeAll(v, Mermaid, pre) => new VisualiseMermaid(() => v(getAll().map(kv => kv._1 -> pre(kv._2))), w._1, out, doc)
      case VisualizeAll(v, Text, pre) => new VisualiseText(() => v(getAll().map(kv => kv._1 -> pre(kv._2))), w._1, out, doc)
      case Visualize(_, Html, _) | VisualizeAll(_, Html, _) =>
        out.setValue("HTML visualiser not supported")
        sys.error("HTML visualiser not supported")
      case VisualizeTab(views, Text, titles, pre) =>
        new Tabs(() => views(pre(get())), w._1, () => titles(pre(get())), "", out, doc) // no language produces text boxes
      case VisualizeTab(views, Code(lang), titles, pre) =>
        new Tabs(() => views(pre(get())), w._1, () => titles(pre(get())), lang, out, doc)
      case VisualizeOpt(view, t, pre) => t match {
        case Mermaid => new VisualiseOptMermaid(() => view(pre(get())), w._1, out, doc)
        case _ => throw new RuntimeException("case not covered...")
      }
      case sim@Simulate(_, _, _, t, _) => t match { // view(pre(get())) match {
        case Text => new SimulateText(get, sim, w._1, out, doc)
        case Mermaid => new SimulateMermaid(get, sim, w._1, out, doc)
        case _ => throw new RuntimeException(s"case not covered when compiling widget '${w._1}': $sim")
      }
      case Explore(init, sos, vS, vA) => new widgets.Explore(() => init(get()), sos, vS, vA, w._1, out, doc)
      case Analyse(a) =>
        new Invisible[Stx, Unit](get, stx => (a(stx), Nil, ()), w._1)
      case _ => throw new RuntimeException(s"case not covered when compiling widget '${w._1}': ${w._2}")
    } catch {
      case e: Throwable =>
        out.error(e.getMessage)
        throw e
    }
  }

  private def cleanContainers(): Unit = {
    val d = dom.document.getElementById("contentWrap")
    val d_nested = dom.document.getElementById("content")
    d.removeChild(d_nested)
  }

  /** This method loads the examples from a local file. */
  @JSExportTopLevel("getFileAsText")
  def getFileAsText[A](ev: dom.File): Unit = {
    val reader = new dom.FileReader()
    reader.readAsText(ev)
    reader.onload = _ => {
      val resultAsString = Utils.unfix(reader.result.toString)
      state.getLastConfig match {
        case c: Configurator[A] @unchecked =>
          val c2: Configurator[A] = new Configurator[A] {
            override val parser: String => A = c.parser
            override val name: String = c.name
            override val languageName: String = c.languageName
            override val setting: Setting = c.setting
            override def widgets: Iterable[(String, Option[WidgetInfo[A]])] = c.widgets
            override val examples: Iterable[Configurator.Example] =
              ExampleWidget.txtToExamples(resultAsString)
          }
          cleanContainers()
          initSite(c2)
        case _ =>
      }
    }
  }
}