package caos.view

/**
 * A `View` is an object that, given an argument, can produce a string that represents it.
 * We consider 2 kinds of views: for textual representation and for diagrams, written
 * using `mermaid` notation.
 *
 * @tparam A is the type of the argument that can be viewed as a String.
 */
case class View(code: String)
// viewtype cannot be hardcoded in the type, since it can be lost by erasure.

//sealed trait View:
//  val code:String

//case class Mermaid(code:String) extends View
//case class Text(code:String)    extends View
//case class Html(code:String)    extends View

sealed abstract class ViewType
case object Mermaid extends ViewType
case object Text    extends ViewType
case object Html    extends ViewType



