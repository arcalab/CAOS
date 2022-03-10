package caos.view

import OptionView._
/**
 * Created by   on 13/07/2021
 */

trait OptionView:
  val options:Options

object OptionView:
  type Options = Map[String,String]
  
  case class OptMermaid(options:Options) extends OptionView
  case class OptText(options:Options)    extends OptionView
  case class OptHtml(options:Options)    extends OptionView  
  
  
