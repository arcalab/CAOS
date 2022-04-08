package caos.frontend.widgets

import caos.view.View
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}


// todo: receive a list of potentially different types of view
//  reorganize boxes to produce an html element and make Box the
//  be the main box that receive an html element and buttons?
case class Tabs(
  tabs:()=>List[View],
  name:String,
  tabsTitle:()=>List[String],
  errorBox: OutputArea
) extends Box[Unit](name, List()) {

    private var box:Block = _
    protected val divBox = name.replace(' ','_') + "Box"

    override val get: Unit = () //mermaid()
    //protected var tabsBlocks:List[Block] = _

    /**
     * Executed once at creation time, to append the content to the inside of this box
     *
     * @param div     Placeholder that will receive the "append" with the content of the box
     * @param visible is true when this box is initially visible (i.e., expanded).
     */
    override def init(div: Block, visible: Boolean): Unit = {
      box = panelBox(div, visible,buttons= Nil).append("div")
//        List(
//        Right("copy")-> (() => Utils.copyText(divBox,Some("tab-content active")), "copy text from active tab")
//        )
        //.attr("class","text")
        .attr("id", divBox)
        //.append("pre")
        //.attr("style","text-align: left;margin: 0;font-size: 1.2rem;")

      dom.document.getElementById(name).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
        .onclick = {(e: MouseEvent) => if(!isVisible) showTabs() }
    }

    /**
     * Block of code that should read the dependencies and:
     *  - update its output value, and
     *  - produce side-effects (e.g., redraw a diagram)
     */
    override def update(): Unit = if(isVisible) showTabs()


    def showTabs():Unit = {
      try {
        box.html("")
        val views = tabs()
        val titles = tabsTitle()
        val tabsBlocks = box.append("ul")
          .attr("class","nav nav-tabs")
        (0 to views.size-1).map(i=>
          tabsBlocks.append("li")
            .attr("class",if i==0 then "active" else "")
            .append("a")
            .attr("data-toggle","tab")
            .attr("href",s"#tab${i+Tabs.index}")
            .text(titles(i))
        )
        val tabContent = box.append("div")
          .attr("class","tab-content")
        val toShow =
          for (tabView,i) <- views.zipWithIndex yield
            val tab = tabContent.append("div")
              .attr("id",s"tab${i+Tabs.index}")
              .attr("class","tab-pane fade" ++ (if i==0 then "in active" else ""))
            tab.append("pre")
              .attr("class","language-scala line-numbers")
              .append("code")
              .attr("class","""language-scala data-prismjs-copy="copy" match-braces""")
              .attr("id",s"pretab${i+Tabs.index}")
              .text(tabView.code)
              scalajs.js.eval(s"""Prism.highlightElement(document.getElementById("pretab${i+Tabs.index}"))""")
        Tabs.index += views.size
      } catch Box.checkExceptions(errorBox,name)
    }
  }

object Tabs {
  private var index = 0
}