package caos.frontend.widgets

import caos.sos.SOS
import caos.view.{OptionView, View, ViewType}

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
  // experiment
  case class VisualizeOpt[Stx,S](v:S=>OptionView,t:ViewType,pre:Stx=>S)
    extends WidgetInfo[Stx]


