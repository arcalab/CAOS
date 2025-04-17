package caos.frontend

import widgets.{CodeWidget, DomElem, DomNode, ExampleWidget, Invisible, OutputArea, SettingWidget, SimulateMermaid, SimulateText, Tabs, Utils, VisualiseCode, VisualiseMermaid, VisualiseOptMermaid, VisualiseText, Widget, WidgetInfo}
import WidgetInfo.*
import caos.view.*
import org.scalajs.dom
import org.scalajs.dom.document

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Site {
  private var leftColumn:  DomElem = _
  private var rightColumn: DomElem = _
  private var errorArea:       OutputArea = _
  private var descriptionArea: OutputArea = _
  private var toReload:   List[() => Unit] = _

  private var lastConfig: Option[Configurator[_]] = None

  private def getLastConfig: Configurator[_] = {
    lastConfig.getOrElse(throw RuntimeException(s"lastConfig is undefined"))
  }

  private def setLastConfig(lastConfig: Configurator[_]): Configurator[_] = {
    Site.lastConfig = Some(lastConfig)
    getLastConfig
  }

  private var codeWidget: Option[CodeWidget[_]] = None

  private def getCodeWidget: CodeWidget[_] = {
    codeWidget.getOrElse(throw RuntimeException(s"codeWidget is undefined"))
  }

  private def setCodeWidget(codeWidget: CodeWidget[_]): CodeWidget[_] = {
    Site.codeWidget = Some(codeWidget)
    getCodeWidget
  }

  private var examplesWidget: Option[ExampleWidget] = None

  private def getExamplesWidget: ExampleWidget = {
    examplesWidget.getOrElse(throw RuntimeException(s"exampleWidget is undefined"))
  }

  private def setExamplesWidget(examplesWidget: ExampleWidget): ExampleWidget = {
    Site.examplesWidget = Some(examplesWidget)
    getExamplesWidget
  }

  private var mainExample: Option[Configurator.Example] = None

  private def getMainExample: Configurator.Example = {
    mainExample.getOrElse(throw RuntimeException(s"mainExample is undefined"))
  }

  private def setMainExample(mainExample: Configurator.Example): Configurator.Example = {
    Site.mainExample = Some(mainExample)
    getMainExample
  }

  private var settingWidget: Option[SettingWidget[_]] = None

  private def getSettingWidget: SettingWidget[_] = {
    settingWidget.getOrElse(throw RuntimeException(s"settingWidget is undefined"))
  }

  private def setSettingWidget(settingWidget: SettingWidget[_]): SettingWidget[_] = {
    Site.settingWidget = Some(settingWidget)
    getSettingWidget
  }

  def getSetting: Setting = {
    getSettingWidget.get
  }

  def setSetting(setting: Setting): Unit = {
    getSettingWidget.set(setting)
    getSettingWidget.update()
    renderWidgets()
  }

  def initSite[A](config: Configurator[A]): Unit = {
    setLastConfig(config)
    initialiseContainers()

    val urlQuery = parseURLQuery
    setMainExample(resolveMainExample(config, urlQuery))

    errorArea       = new OutputArea
    descriptionArea = new OutputArea

    setCodeWidget(mkCodeBox(config, Some(getMainExample)))
    getCodeWidget.init(leftColumn, true)

    errorArea.init(leftColumn)

    setSettingWidget(mkSettingBox(config))
    getSettingWidget.init(leftColumn, true)

    document.getElementById("title").textContent      = config.name
    document.getElementById("tool-title").textContent = config.name

    setExamplesWidget(new ExampleWidget("Examples", config, globalReload(), getCodeWidget, Some(descriptionArea), getSettingWidget))

    descriptionArea.init(leftColumn)
    getExamplesWidget.init(leftColumn, true)

    applySettingAndDescription(descriptionArea)

    renderWidgets()
  }

  private def parseURLQuery: String = {
    document.URL.split('?').drop(1).mkString("?")
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

  private def applySettingAndDescription(descriptionArea: OutputArea): Unit = {
    getMainExample match {
      case example if example.description.nonEmpty =>
        descriptionArea.setValue(example.description)
        getSettingWidget.set(example.setting.getOrElse(Setting()))
        getSettingWidget.update()
      case _ =>
    }
  }

  private def renderWidgets(): Unit = {
    val config = getLastConfig
    val code   = getCodeWidget

    val boxes = for wc <- config.widgets if wc._2.getRender yield
      val w = mkWidget(
        wc,
        () => code.get.asInstanceOf[config.StxType],
        () => getExamplesWidget.get.map(kv => kv._1 -> config.parser(kv._2)),
        errorArea,
        config.documentation
      )
      // place widget in the document
      w.init(if wc._2.location == 0 then rightColumn else leftColumn, wc._2.expanded)
      w

    toReload = (List(code) ++ boxes).map(b => () => b.update()) ++ List(getSettingWidget.partialReload)
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
  private def mkWidget[Stx](w: (String, WidgetInfo[Stx])
                            , get: () => Stx
                            , getAll: () => Seq[(String, Stx)]
                            , out: OutputArea
                            , doc: Documentation
                           ): Widget[Unit] = {
    try w._2 match {
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
    val d = document.getElementById("contentWrap")
    val d_nested = document.getElementById("content")
    d.removeChild(d_nested)
  }


  private def initialiseContainers(): Unit = {
    val contentDiv = DomNode.select("contentWrap").append("div")
      .attr("class", "content")
      .attr("id", "content")

    val rowDiv = contentDiv.append("div")
      .attr("id", "mytable")

    leftColumn = rowDiv.append("div")
      .attr("id", "leftbar")
      .attr("class", "leftside")

    leftColumn.append("div")
      .attr("id", "dragbar")
      .attr("class", "middlebar")

    rightColumn = rowDiv.append("div")
      .attr("id", "rightbar")
      .attr("class", "rightside")

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

    if getLastConfig.footer != ""
    then contentDiv.append("div")
      .style("width: 100%;text-align: center; display: inline-block;")
      .html(s"&nbsp;<br><p style=\"margin: 0px 30px 10px;\">${getLastConfig.footer}</p>")

    Utils.resizeCols
  }

  private def globalReload(): Unit = {
    errorArea.clear()
    toReload.foreach(f => f())
  }

  private def mkCodeBox[A](config: Configurator[A]
                           , ex: Option[Configurator.Example]): CodeWidget[A] = {
    new CodeWidget[A](config.languageName, Nil) {
      protected var input: String = ex match
        case Some(e) => e.example
        case _ => ""

      override protected val boxId: String = config.name + "Box"

      override protected val buttons: List[(Either[String, String], (() => Unit, String))] = {
        List(
          Right("refresh") -> (
            () => reload(),
            s"Load the ${config.languageName} program (shift-enter)"
          ),
          Right("download") -> (
            () => Utils.downloadTxt(ExampleWidget.examplesToTxt(getExamplesWidget.getCurrentExample), "example.txt"),
            "Download Current Example"
          ),
        ) ::: Widget.mkHelper(config.languageName, config.documentation).toList
      }

      override def get: A = config.parser(input)

      override protected val codemirror: String = "caos" //config.name.toLowerCase()

      override def reload(): Unit =
        descriptionArea.clear()
        update()
        globalReload()
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

      override def partialReload(): Unit = {
        errorArea.clear()
        update()
        document.getElementById("rightbar").innerHTML = ""
        renderWidgets()
      }

      override def reload(): Unit = {
        descriptionArea.clear()
        errorArea.clear()
        update()
        document.getElementById("rightbar").innerHTML = ""
        renderWidgets()
      }
    }
  }

  /** This method loads the examples from a local file. */
  @JSExportTopLevel("getFileAsText")
  def getFileAsText[A](ev: dom.File): Unit = {
    val reader = new dom.FileReader()
    reader.readAsText(ev)
    reader.onload = _ => {
      val resultAsString = Utils.unfix(reader.result.toString)
      getLastConfig match {
        case c: Configurator[A] @unchecked =>
          val c2 = new Configurator[A] {
            override val parser: String => A = c.parser
            override val name: String = c.name
            override val languageName: String = c.languageName
            override val setting: Setting = c.setting
            override val widgets: Iterable[(String, WidgetInfo[A])] = c.widgets
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