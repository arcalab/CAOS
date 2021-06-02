package caos.frontend.widgets

import caos.frontend.widgets.Setable
import caos.common.Example

/**
 * Created by guillecledou on 07/05/2021
 */
class ExampleBox(title:String
                 , examples:Iterable[Example] //Seq[List[String]]
                 , reload: => Unit
                 , setableExample:Setable[String]
                 , setableDescription:Option[Setable[String]]=None)
  extends Box[Unit](title, Nil) {


  override def get: Unit = ()

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    val buttonsDiv = super.panelBox(div,visible).append("div")
      .attr("id", "buttons")
      .attr("style","padding: 2pt;")

    buttonsDiv
      .style("display:block; padding:2pt")

    for (ex <- examples ) yield genButton(ex,buttonsDiv)
  }


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