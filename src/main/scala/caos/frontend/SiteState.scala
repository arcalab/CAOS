package caos.frontend

import caos.frontend.widgets.{DomElem, OutputArea, CodeWidget, ExampleWidget, SettingWidget, Widget}


case class SiteState(leftColumn:      Option[DomElem] = None,
                     rightColumn:     Option[DomElem] = None,
                     errorArea:       Option[OutputArea] = None,
                     descriptionArea: Option[OutputArea] = None,
                     toReload:        List[() => Unit] = List.empty,
                     lastConfig:      Option[Configurator[_]] = None,
                     codeWidget:      Option[CodeWidget[_]] = None,
                     examplesWidget:  Option[ExampleWidget] = None,
                     settingWidget:   Option[SettingWidget[_]] = None,
                     previouslyExpandedWidgets: Set[Widget[_]] = Set.empty
                    ) {
  def getLeftColumn: DomElem = {
    this.leftColumn.get
  }

  def withLeftColumn(leftColumn: DomElem): SiteState = {
    this.copy(leftColumn = Some(leftColumn))
  }

  def getRightColumn: DomElem = {
    this.rightColumn.get
  }

  def withRightColumn(rightColumn: DomElem): SiteState = {
    this.copy(rightColumn = Some(rightColumn))
  }

  def getErrorArea: OutputArea = {
    this.errorArea.get
  }

  def withErrorArea(errorArea: OutputArea): SiteState = {
    this.copy(errorArea = Some(errorArea))
  }

  def getDescriptionArea: OutputArea = {
    this.descriptionArea.get
  }

  def withDescriptionArea(descriptionArea: OutputArea): SiteState = {
    this.copy(descriptionArea = Some(descriptionArea))
  }

  def getToReload: List[() => Unit] = {
    this.toReload
  }

  def withToReload(toReload: List[() => Unit]): SiteState = {
    this.copy(toReload = toReload)
  }

  def getLastConfig: Configurator[_] = {
    this.lastConfig.get
  }

  def withLastConfig(lastConfig: Configurator[_]): SiteState = {
    this.copy(lastConfig = Some(lastConfig))
  }

  def getCodeWidget: CodeWidget[_] = {
    this.codeWidget.get
  }

  def withCodeWidget(codeWidget: CodeWidget[_]): SiteState = {
    this.copy(codeWidget = Some(codeWidget))
  }

  def getExamplesWidget: ExampleWidget = {
    this.examplesWidget.get
  }

  def withExamplesWidget(examplesWidget: ExampleWidget): SiteState = {
    this.copy(examplesWidget = Some(examplesWidget))
  }

  def getSettingWidget: SettingWidget[_] = {
    this.settingWidget.get
  }

  def withSettingWidget(settingWidget: SettingWidget[_]): SiteState = {
    this.copy(settingWidget = Some(settingWidget))
  }

  def getPreviouslyExpandedWidgets: Set[Widget[_]] = {
    this.previouslyExpandedWidgets
  }

  def withPreviouslyExpandedWidgets(previouslyExpandedWidgets: Set[Widget[_]]): SiteState = {
    this.copy(previouslyExpandedWidgets = previouslyExpandedWidgets)
  }
}