package caos.frontend.widgets

class HtmlBlock(block:String, title: String) extends Widget[Unit](title):
  //val content = new OutputArea

  override def get: Unit = {}

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible ignored, since this box will never be hidden.
   * @param hidden  if true, the box is hidden.
   */
  override def init(div: Block, visible: Boolean, hidden: Boolean): Unit =
    val content = div.append("div").html(block)
    if hidden then content.style("display","none")
//    content.outputs.style("padding: 0px 5px 0px 5px;")
//    update()

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit = {}

  override protected def panelBox(parent:Block,
                         visible:Boolean,
                         hidden:Boolean = false,
                         headerStyle: List[(String,String)] = Nil,
                         buttons:List[(Either[String,String], (()=>Unit,String) )] = Nil) : Block =
    parent


