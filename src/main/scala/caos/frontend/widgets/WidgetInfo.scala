package caos.frontend.widgets

import caos.frontend.widgets.Widget.Helper
import caos.sos.SOS
import caos.view.{OptionView, View, ViewType}

/**
 * Description of a widget to be created by CAOS.
 * @tparam Stx Type of the data structure being analysed.
 */
sealed trait WidgetInfo[-Stx]:
  var expanded = false
  var location = 0
  /** Sets whether the widget is initially collapsed or expanded */
  def expand: WidgetInfo[Stx] = {expanded = true; this}
  /** Sets the location where the widget should be placed */
  def moveTo(i:Int): WidgetInfo[Stx] = {location = i; this}

object WidgetInfo:
  case class Visualize[Stx,S](v:S=>View, typ:ViewType, pre:Stx=>S)
    extends WidgetInfo[Stx]
  case class VisualizeAll[Stx,S](v:Seq[(String,S)]=>View, typ:ViewType, pre:Stx=>S)
    extends WidgetInfo[Stx]
  case class Simulate[Stx,A,S](sos:SOS[A,S],v:S=>View,lb:A=>String,typ:ViewType,pre:Stx=>S)
    extends WidgetInfo[Stx]
  case class Explore[Stx,A,S](pre:Stx=>S,sos:SOS[A,S],vS:S=>String,vA:A=>String)
    extends WidgetInfo[Stx]
  case class VisualizeTab[Stx,S](v:S=>List[View],typ:ViewType,t:S=>List[String],pre:Stx=>S)
    extends WidgetInfo[Stx]
  case class VisualizeWarning[Stx,S](v:S=>View, typ:ViewType, pre:Stx=>S)
    extends WidgetInfo[Stx]
  case class Analyse[Stx](a:Stx=>Seq[String])
    extends WidgetInfo[Stx]
  // experiment
  case class VisualizeOpt[Stx,S](v:S=>OptionView,t:ViewType,pre:Stx=>S)
    extends WidgetInfo[Stx]


