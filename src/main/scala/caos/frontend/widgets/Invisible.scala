package caos.frontend.widgets

class Invisible[A,B](stx:()=>A, analyse: A => (Seq[String],Seq[String], B),title:String) extends Widget[B](title):
  val content = new OutputArea

  var lastB: Option[B] = None

  override def get: B = lastB match
    case Some(b) => b
    case None => sys.error("Could not get a value from analyser - not yet initialised.")

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible ignored, since this box will never be hidden.
   */
  override def init(div: Block, visible: Boolean): Unit =
    content.init(div)
    content.outputs.style("padding: 0px 5px 0px 5px;")
    update()

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit =
    content.clear()
    try {
      val (warns,errs,res) = analyse(stx())
      for e<-errs do content.error(e)
      for w<-warns do content.warning(w)
      lastB = Some(res)
    } catch Widget.checkExceptions(content,title)


  override protected def panelBox(parent:Block,
                         visible:Boolean,
                         headerStyle: List[(String,String)] = Nil,
                         buttons:List[(Either[String,String], (()=>Unit,String) )] = Nil) : Block =
    parent


