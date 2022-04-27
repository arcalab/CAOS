package caos.frontend.widgets

import caos.sos.SOS
import caos.view.{OptionView, View, ViewType}

/**
 * Description of a widget to be created by CAOS.
 * @tparam Stx Type of the data structure being analysed.
 */
sealed abstract class WidgetInfo[Stx]

object WidgetInfo:
  case class Visualize[Stx,S](v:S=>View, typ:ViewType, pre:Stx=>S)
    extends WidgetInfo[Stx]
  case class Simulate[Stx,A,S](sos:SOS[A,S],v:S=>View,typ:ViewType,pre:Stx=>S)
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


