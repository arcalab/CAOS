package caos.view

/**
 * A `View` is an object that, given an argument, can produce a string that represents it.
 * We consider 2 kinds of views: for textual representation and for diagrams, written
 * using `mermaid` notation.
 *
 * @tparam A is the type of the argument that can be viewed as a String.
 */
sealed trait View:
  val code:String

case class Mermaid(code:String) extends View
case class Text(code:String)    extends View
case class Html(code:String)    extends View
