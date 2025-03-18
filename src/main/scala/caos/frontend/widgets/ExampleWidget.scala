package caos.frontend.widgets

import caos.frontend.{Configurator, SettingParser, Setting}
import caos.frontend.widgets.Setable
import caos.frontend.Configurator.Example
import org.scalajs.dom
import org.scalajs.dom.{DOMException, Event, FileReader, UIEvent}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel


class ExampleWidget(title:String
                    , config:Configurator[_]
                    , reload: => Unit
                    , setableExample:Setable[String]
                    , setableDescription:Option[Setable[String]]=None
                    , settingWidget: SettingWidget[_])
  extends Widget[Seq[(String,String)]](title) {

  private val examples = config.examples

  private var currentExample: Option[Example] = config.examples.headOption

  def getCurrentExample: Option[Example] = currentExample

  override def get: Seq[(String, String)] = examples.map(e => e.name -> e.example).toSeq

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */

  override def init(div: Block, visible: Boolean): Unit = {
    val buttonsDiv = super.panelBox(div,visible,buttons=buttons).append("div")
      .attr("id", "buttons")
      .attr("style","padding: 2pt;")
      .style("display:block; padding:2pt")

    for (ex <- examples ) yield genButton(ex, buttonsDiv)
  }

  private def buttons: List[(Either[String, String], (() => Unit, String))] = {
    List(
      Right("upload") -> (
        () => Utils.uploadTxt(),
        "Upload Examples"
      ),
      Right("download") -> (
        () => Utils.downloadTxt(ExampleWidget.examplesToTxt(examples), "examples.txt"),
        "Download Examples"
      ),
    ) ++ Widget.mkHelper(title, config.documentation)
  }

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side effects (e.g., redraw a diagram)
   */
  override def update(): Unit = ()

  private def genButton(ex:Example,buttonsDiv:Block): Unit = {
    val button = buttonsDiv.append("button").textEl(ex.name)
    button.on("click",() => {
      currentExample = Some(ex)
      setableExample.setValue(ex.example)
      ex.setting match
        case Some(setting) =>
          settingWidget.set(setting)
          settingWidget.update()
        case None =>
      for sd <- setableDescription yield
        sd.setValue(ex.description)
      reload
    })
  }

  /** Applies a button, if it exists.
   * @param button name of the button to be applied
   */
  def loadButton(button:String): Boolean = {
    examples.find(ex=>ex.name == button) match {
      case Some(ex) =>
        currentExample = Some(ex)
        setableExample.setValue(ex.example)
        ex.setting match
          case Some(setting) =>
            settingWidget.set(setting)
            settingWidget.update()
          case None =>
        for sd <- setableDescription yield
          sd.setValue(ex.description)
        reload
        true
      case _ => false
    }
  }
}

object ExampleWidget {
  def updateExamples(): Unit = {
    val bt = dom.document.getElementById("buttons")
    val btt = DomNode(bt)
  }

  def txtToExamples(str: String): Iterable[Example] = {
    val list = str.split("module *")
    for (ex <- list if ex != "") yield {
      try {
        val (name, rest) = ex.span(_ != ':')
        val rest2 = rest.drop(18) // drop "description"
        val (desc, rest3) = rest2.span(_ != '\n')
        val (code, rest4) = rest3.span(_ != '@')
        val setting = rest4.tail
        Example(unfix(code.trim), unfix(name.trim), unfix(desc.trim), Some(SettingParser(unfix(setting.trim))))
      } catch {
        case e:Throwable => throw new RuntimeException(s"Failed to import when reading: $ex")
      }
    }
  }

  def examplesToTxt(examples: Iterable[Example]): String = {
    examples.map(e =>
      s"module ${e.name}:\\n" +
        s"// description: ${fix(e.description)}\\n" +
        s"${fix(e.example)}\\n" +
        s"@${fix(e.setting.getOrElse(Setting()).toStringRaw)}")
      .mkString("\\n\\n")
  }

  private def fix(unfixedString: String): String = {
    unfixedString
      .replaceAll("\\\\n","§NL;") // replaced in UTILS
      .replaceAll("module","§MODL;")
      .replaceAll("@","§AT;")
  }

  private def unfix(fixedString: String): String = {
    fixedString
      .replaceAll("§MODL;","module")
      .replaceAll("§AT;","@")
  }
}