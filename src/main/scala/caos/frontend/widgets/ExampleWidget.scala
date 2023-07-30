package caos.frontend.widgets

import caos.frontend.Configurator
import caos.frontend.widgets.Setable
import caos.frontend.Configurator.Example
import org.scalajs.dom
import org.scalajs.dom.{DOMException, Event, FileReader, UIEvent}
//import org.w3c.dom.DOMError

//import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/**
 * Created by   on 07/05/2021
 */
class ExampleWidget(title:String
                    , config:Configurator[_] // examples:Iterable[Example] //Seq[List[String]]
                    , reload: => Unit
                    , setableExample:Setable[String]
                    , setableDescription:Option[Setable[String]]=None)
  extends Widget[Seq[(String,String)]](title) {

  private val examples = config.examples

  override def get: Seq[(String,String)] = examples.map(e=>e.name->e.example).toSeq

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

//    val inp = buttonsDiv.append("input")
//      .attr("type","file")
//      .attr("id","file")
//      .attr("name","files[]")
//
//    import concurrent.ExecutionContext.Implicits.global
//    def printFileContent(file: dom.File) =
//      readTextFile(file).map {
//        case Right(fileContent) => println(s"File content: $fileContent")
//        case Left(error) => println(s"Could not read file ${file.name}. Error: $error")
//      }
//
//    /** In the future, returns either the file's content or an error,
//    if something went wrong */
//    def readTextFile(fileToRead: dom.File): Future[Either[DOMException, String]] = {
//
//      // Used to create the Future containing either the file content or an error
//      val promisedErrorOrContent = Promise[Either[DOMException, String]]
//
//      val reader = new FileReader()
//      reader.readAsText(fileToRead, "UTF-8")
//
//      reader.onload = (_) => {
//        val resultAsString = s"${reader.result}"
//        promisedErrorOrContent.success(Right(resultAsString))
//      }
//      reader.onerror = (_: Event) => promisedErrorOrContent.success(Left(reader.error))
//
//      promisedErrorOrContent.future
//    }
////    val reader = new dom.FileReader()
////    reader.readAsText(e.currentTarget.files.item(0))
////    reader.onload(
////    scalajs.js.eval(
////      """function startRead(evt) {
////        |    var file = document.getElementById('file').files[0];
////        |    if (file) {
////        |        //  getAsText(file);
////        |        alert("Name: " + file.name + "\n" + "Last Modified Date :" + file.lastModifiedDate);
////        |    }
////        |}""".stripMargin
////    )

    buttonsDiv
      .style("display:block; padding:2pt")

    for (ex <- examples ) yield genButton(ex,buttonsDiv)
  }

//  @JSExportTopLevel("loadedFile")
//  def loadedFile(ev: dom.Event): Unit = {
//    println("File Loaded Successfully");
//    var fileString = ev.target.toString;
//    println(fileString);
////    $$("#op").text(fileString);
////    appendPar(document.body, "You clicked the button!")
//  }

  private def buttons = List(
    Right("upload")-> (
      () => Utils.uploadTxt() , //Utils.downloadTxt(s"examples: ${examples.map(_.name).mkString(", ")}", "examples.txt"),
      "Upload Examples"),
    Right("download")-> (
      () => Utils.downloadTxt(ExampleWidget.examplesToTxt(examples),"examples.txt"),
      "Download Examples")
  ) ++
    Widget.mkHelper(title,config.documentation)

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit = ()

  protected def genButton(ex:Example,buttonsDiv:Block): Unit = {
    val button = buttonsDiv.append("button").textEl(ex.name)
    button.on("click",() => {
      setableExample.setValue(ex.example)
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
        setableExample.setValue(ex.example)
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
//    btt.append()
  }

  def txtToExamples(str:String): Iterable[Example] = {
    val list = str.split("module *")
      for (ex <- list if ex != "") yield {
        try {
          val (name, rest) = unfix(ex).span(_ != ':')
          val rest2 = rest.drop(18) // drop "description"
          val (desc, rest3) = rest2.span(_ != '\n')
          val code = rest3.tail
          Example(code.trim, name.trim, desc.trim)
        } catch {
          case e:Throwable => throw new RuntimeException(s"Failed to import when reading: $ex")
        }
      }
  }

  def examplesToTxt(examples:Iterable[Example]): String =
    examples.map(e => s"module ${e.name}:\\n// description: ${
      fix(e.description)}\\n${fix(e.example)}").mkString("\\n\\n")

  private def fix(s:String): String = s
      .replaceAll("\\\\n","§NL;") // replaced in UTILS
//      .replaceAll("\"","\\\"")
      .replaceAll("module","§MODL;")

  private def unfix(s:String): String = s
//    .replaceAll("\\\\n","\\n")
//    .replaceAll("\\\"","\"")
    .replaceAll("§MODL;","module")


}