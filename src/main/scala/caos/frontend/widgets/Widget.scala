package caos.frontend.widgets

import caos.frontend.widgets.{DomElem, DomNode}
import Widget.Block
import caos.frontend.Documentation
import org.scalajs.dom
import org.scalajs.dom.{EventTarget, MouseEvent, html}

import scala.annotation.tailrec
import scala.scalajs.js.{JavaScriptException, UndefOr}


//panel boxes are the abstract entities which contain each panel displayed on the website
abstract class Widget[A](val title: String, doc: Documentation = Documentation()){
  type Block = DomElem //Selection[dom.EventTarget]

  protected val titleId = "id"+title.hashCode
  var wrap:DomElem = _

  /**
   * Creates a collapsable pannel
   * */
  protected def panelBox(parent:Block,
                         visible:Boolean,
                         headerStyle: List[(String,String)] = Nil,
                         buttons:List[(Either[String,String], (()=>Unit,String) )] = Nil) : Block = {
    //    val percentage=100

    //var expander: Block = parent
    wrap = parent.append("div").attr("class","panel-group")
      .append("div").attr("class","panel panel-default").attr("id",titleId)
    var expander = wrap
      .append("div").attr("class", "panel-heading my-panel-heading")
      .append("h4")
      .attr("class", "panel-title")
      .attr("style",s"padding-right: ${28*buttons.size}pt;")
    //        .append("table")
    //        .attr("width", "100%")
    //        .append("th")
    //      .attr("width", s"$percentage%")
    for ((s,v)<-headerStyle)
      expander.style(s,v)
    expander = expander
      .append("a")
      .attr("data-toggle", "collapse")
      .attr("href", "#collapse-1" + titleId)
      .attr("aria-expanded", visible.toString)
    if(!visible)
      expander.attr("class","collapsed")
    expander
      .text(title)
    val res = wrap
      .append("div").attr("id","collapse-1"+titleId)
      .attr("class",if (visible) "panel-collapse collapse in" else "panel-collapse collapse")
      .attr("style",if (visible) "" else "height: 0px;")
      .attr("aria-expanded",visible.toString)
      .append("div").attr("class","panel-body my-panel-body")


    val allButtons =
      Widget.mkHelper(title,doc).toList:::(buttons.reverse)

    // Buttons
    for ((name,(action,title)) <- allButtons) {
      //      .append("button").attr("class", "btn btn-default btn-sm")
      //        .style("float","right")
      //        .style("margin-top","-15pt")
      //        .style("display","flex")

      val button = wrap
        .select("div")
        .append("button").attr("class", "btn btn-default btn-sm")
        .style("float","right")
        .style("margin-top","-15.5pt")
        .style("max-height","18pt")
        .style("margin-left","0pt")
        .style("display","flex")
        .style("border", "none")
        .style("background-image", "none")
        .style("text-shadow", "none")
        .style("box-shadow", "none")
        .style("background-color", "transparent")
        .style("padding", "5px 6px 5px 4px")

      if (name==Right("help")) button
        .style("margin-left","-2pt")
        .style("border", "none")
        .style("background", "none")
        .style("box-shadow", "none")
        .style("padding", "3pt")
        .style("margin-top","-16.6pt")

      if (title.nonEmpty) button.attr("title",title)

      drawButton(button,name)

      //      button.on("click", {(e: EventTarget, a: Int, b:UndefOr[Int])=> { action() }})
      // EXPERIMENT
      button.on("click", ()=>action() )
    }

    res
  }

  @tailrec
  private def drawButton(button:Block, info:Either[String,String]): Unit = {
    info match {
      case Left(str) =>
        val b = button.append("span")
        b.style("line-height","9pt")
        b.html(str)
      case Right("upload") =>
        Widget.uploadSvg(button)
      case Right("download") =>
        Widget.downloadSvg(button)
      //        val svg = button.append("img")
      //          .attr("src","assets/content/svg/cloud_download.svg")
      //          .style("width","15pt")
      case Right("refresh") =>
        button.append("span").attr("class", "glyphicon glyphicon-refresh")
      case Right("help") =>
        Widget.helpSvg(button)
      case Right("helpOld") =>
        val b = button.append("span")
        b .style("line-height","9pt")
          .style("padding","0pt 0pt 4pt 3pt")
          .style("color", "#b0b0b0")
          .style("text-shadow", "none")
        b.html("?")
      //        button.append("img")
      //          .attr("src","svg/help.svg")
      //          .style("margin","-2pt -2pt 0pt -2pt")
      case Right("oldDownload") => drawButton(button,Left("&dArr;"))
      case Right(s) => drawButton(button,Left(s))
    }
  }

  def isVisible: Boolean = {
    val es = dom.document.getElementsByClassName("collapsed")
    var foundId = false
    for (i <- 0 until es.length) {
      // println(es.item(i).parentNode.parentNode.parentNode.attributes.getNamedItem("id").value)
      //      println("### - "+es.item(i).parentNode.parentNode.parentNode.attributes.getNamedItem("id").value)
      foundId = foundId || es.item(i).parentNode.parentNode.parentNode.attributes.getNamedItem("id").value == titleId
    }

    //    println("### - "+es.length)
    //    println("### - "+es.item(0).localName)
    //    println("### - "+es.item(0).parentNode.localName)
    //    println("### - "+es.item(0).parentNode.parentNode.localName)
    //    println("### - "+es.item(0).parentNode.parentNode.parentNode.attributes.getNamedItem("id").value)

    //    val res = expander.attr("aria-expander") == "true"
    //    println("--- "+expander.html().render)
    //    println("--- "+expander.classed("collapsed"))
    //    println("--- "+expander.attr("aria-expander"))
    //    println("$$$ "+ (!foundId))
    !foundId
  }

  // add actions in "init" to update to visibility toggles
  def toggleVisibility(visible:()=>Unit = ()=>{}, invisible:()=>Unit = ()=>{}): Unit =
    dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = {(_: MouseEvent) => if(!isVisible) visible() else invisible()}


  def get: A

  /**
   * Executed once at creation time, to append the content to the inside of this box
   * @param div Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  def init(div: Block, visible: Boolean): Unit

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  def update(): Unit

  /** Adds code that is executed everyting the text of the title is clicked.
   * The code is executed *before* collapsing/expanding. */
  def whenClickTitle(update: ()=>Unit): Unit =
    dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
      .onclick = {(_: MouseEvent) => update()}

}

object Widget {
  type Block = DomElem //Selection[dom.EventTarget]
  type Helper = Option[(String,String)]

  def mkHelper(title:String, doc:Documentation): Option[(Either[String, String], (() => Unit, String))] =
    //if doc.get(title).isEmpty then println(s"Not found documentation for '$title'. Only for ${doc.widgets.map(x=>"\n - "+x).mkString}")
    doc.get(title).map( hlp =>
      (Right("help") -> (() =>{
//      org.scalajs.dom.window.open(hlp.get._2)
      dom.document.getElementById("CAOSPopup").innerHTML = hlp._2
      dom.document.getElementById("CAOSPopupTitle").innerHTML = title
      dom.document.getElementById("CAOSOverlay").setAttribute("style","display:block;")
      dom.document.getElementById("CAOSPopupWrp").setAttribute("style","display:block;")
      dom.document.location.replace(s"#")
    }, hlp._1
    )))

  def downloadSvgOld(block: Block): Unit = {
    val svg = block.append("svg")
      .attr("xmlns","http://www.w3.org/2000/svg")
      .attr("width","20")
      .attr("height","20")
      .attr("viewBox","0 0 24 24")
      .attr("class", "svgIcon")
    svg.style("margin","-3pt -2pt 0pt")
    //svg.style("fill","#505050")
    svg.append("path")
      .attr("d","M0 0h24v24H0z")
      .attr("fill","none")
    svg.append("path")
      .attr("d","M19.35 10.04C18.67 6.59 15.64 4 12 4 9.11 4 6.6 5.64 5.35 8.04 2.34 8.36 0 10.91 0 14c0 3.31 2.69 6 6 6h13c2.76 0 5-2.24 5-5 0-2.64-2.05-4.78-4.65-4.96zM17 13l-5 5-5-5h3V9h4v4h3z")
  }

  def downloadSvg(block: Block): Unit = {
    val svg = block.append("svg")
      .attr("xmlns","http://www.w3.org/2000/svg")
      .attr("width","20")
      .attr("height","20")
      .attr("viewBox","0 0 24 24")
      .attr("class", "svgIcon")
    svg.style("margin","-3pt -2pt 0pt")
    //svg.style("fill","#505050")
    svg.append("path")
      .attr("d","M0 0h24v24H0z")
      .attr("fill","none")
    svg.append("path")
      .attr("d","M 11 2 C 10.448 2 10 2.448 10 3 L 10 11 L 6 11 L 12 17 L 18 11 L 14 11 L 14 3 C 14 2.448 13.552 2 13 2 L 11 2 z M 2 20 L 2 22 L 22 22 L 22 20 L 2 20 z")
  }

  def uploadSvg(block: Block): Unit = {
    val svg = block.append("svg")
      .attr("xmlns","http://www.w3.org/2000/svg")
      .attr("width","20")
      .attr("height","20")
      .attr("viewBox","0 0 24 24")
      .attr("class", "svgIcon")
    svg.style("margin","-3pt -2pt 0pt")
    //svg.style("fill","#505050")
    svg.append("path")
      .attr("d","M0 0h24v24H0z")
      .attr("fill","none")
    svg.append("path")
      .attr("d","M 12 2 A 1 1 0 0 0 11.292969 2.2949219 L 6.1601562 7.1347656 A 0.5 0.5 0 0 0 6.1484375 7.1445312 L 6.1464844 7.1464844 A 0.5 0.5 0 0 0 6 7.5 A 0.5 0.5 0 0 0 6.5 8 L 10 8 L 10 16 C 10 16.552 10.448 17 11 17 L 12 17 L 13 17 C 13.552 17 14 16.552 14 16 L 14 8 L 17.5 8 A 0.5 0.5 0 0 0 18 7.5 A 0.5 0.5 0 0 0 17.853516 7.1464844 L 17.822266 7.1171875 L 12.716797 2.3027344 A 1 1 0 0 0 12.683594 2.2714844 A 1 1 0 0 0 12 2 z M 3 20 A 1.0001 1.0001 0 1 0 3 22 L 21 22 A 1.0001 1.0001 0 1 0 21 20 L 3 20 z")
  }

  def helpSvg(block: Block): Unit = {
    val svg = block.append("svg")
      .attr("xmlns", "http://www.w3.org/2000/svg")
      .attr("width", "20")
      .attr("height", "20")
      .attr("viewBox", "0 0 24 24")
      .attr("fill", "none")
      .attr("class", "svgIcon")
    svg.append("path")
      .attr("d","M21 12C21 16.9706 16.9706 21 12 21C7.02944 21 3 16.9706 3 12C3 7.02944 7.02944 3 12 3C16.9706 3 21 7.02944 21 12Z")
      .attr("stroke","#b0b0b0")
      .attr("stroke-width","2")
    svg.append("path")
      .attr("d", "M10.5 8.67709C10.8665 8.26188 11.4027 8 12 8C13.1046 8 14 8.89543 14 10C14 10.9337 13.3601 11.718 12.4949 11.9383C12.2273 12.0064 12 12.2239 12 12.5V12.5V13")
      .attr("stroke", "#b0b0b0")
      .attr("stroke-width", "2")
      .attr("stroke-linecap", "round")
      .attr("stroke-linejoin", "round")
    svg.append("path")
      .attr("d", "M12 16H12.01")
      .attr("stroke", "#b0b0b0")
      .attr("stroke-width", "2")
      .attr("stroke-linecap", "round")
      .attr("stroke-linejoin", "round")
  }


  /**
   * Default function that catches exceptions and produces an error message based on the type of exception.
   * @param errorBox is the placeholder where the exception will be appended to.
   * @return the function to be placed at a catch point.
   */
  def checkExceptions(errorBox: OutputArea, source:String = ""): PartialFunction[Throwable,Unit] = {
    val by = if (source.nonEmpty) s" by '$source''" else ""
    val f: PartialFunction[Throwable,Unit] = {
      // type error
      case e: JavaScriptException =>
        //      val sw = new StringWriter
        //      e.printStackTrace(new PrintWriter(sw))
        //      errorBox.error(/*Show(result)+ */ "JavaScript error : " + e + " - " + e.getClass + "\n" + sw.toString )
        errorBox.error(/*Show(result)+ */ s"JavaScript error$by: $e - ${e.getClass}") // - ${e.getStackTrace.mkString("\n")}")
      //            instanceInfo.append("p").text("-")
      case e: java.lang.AssertionError => errorBox.error(e.getMessage)
      case e: RuntimeException => errorBox.error(s"Error raised$by: " + e.getMessage)
      case e: Throwable => errorBox.error(s"Error$by: " + e + " - " + e.getClass +"\n ### "+e.getStackTrace.mkString("\n - "))
    }
    f
  }

}