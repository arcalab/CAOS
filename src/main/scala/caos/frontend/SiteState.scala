package caos.frontend

import caos.frontend.widgets.{CodeWidget, DomElem, ExampleWidget, OutputArea, SettingWidget}


case class SiteState(
                    private val leftColumn:  DomElem,
                    private val rightColumn: DomElem,
                    private val errorArea:       OutputArea,
                    private val descriptionArea: OutputArea,
                    private val toReload:      List[() => Unit],
                    private val lastConfig:    Option[Configurator[_]],
                    private val codeWidget:    Option[CodeWidget[_]],
                    private val exampleWidget: Option[ExampleWidget],
                    private val mainExample:   Option[Configurator.Example],
                    private val settingWidget: Option[SettingWidget[_]]
                    ) {
  def getLeftColumn: DomElem = {
    leftColumn
  }

  def setLeftColumn(leftColumn: DomElem): SiteState = {
    this.copy(leftColumn = leftColumn)
  }

  def getRightColumn: DomElem = {
    rightColumn
  }

  def setRightColumn(rightColumn: DomElem): SiteState = {
    this.copy(rightColumn = rightColumn)
  }

  def getErrorArea: OutputArea = {
    errorArea
  }

  def setErrorArea(errorArea: OutputArea): SiteState = {
    this.copy(errorArea = errorArea)
  }

  def getDescriptionArea: OutputArea = {
    descriptionArea
  }

  def setDescriptionArea(descriptionArea: OutputArea): SiteState = {
    this.copy(descriptionArea = descriptionArea)
  }

  def getToReload: List[() => Unit] = {
    toReload
  }

  def setToReload(toReload: List[() => Unit]): SiteState = {
    this.copy(toReload = toReload)
  }

  def getLastConfig: Configurator[_] = {
    lastConfig.getOrElse(throw RuntimeException(s"[lastConfig] is undefined"))
  }

  def setLastConfig(lastConfig: Configurator[_]): SiteState = {
    this.copy(lastConfig = Some(lastConfig))
  }

  def getCodeWidget: CodeWidget[_] = {
    codeWidget.getOrElse(throw RuntimeException(s"[codeWidget] is undefined"))
  }

  def setCodeWidget(codeWidget: CodeWidget[_]): SiteState = {
    this.copy(codeWidget = Some(codeWidget))
  }

  def getExampleWidget: ExampleWidget = {
    exampleWidget.getOrElse(throw RuntimeException(s"[exampleWidget] is undefined"))
  }

  def setExampleWidget(exampleWidget: ExampleWidget): SiteState = {
    this.copy(exampleWidget = Some(exampleWidget))
  }

  def getMainExample: Configurator.Example = {
    mainExample.getOrElse(throw RuntimeException(s"[mainExample] is undefined"))
  }

  def setMainExample(mainExample: Configurator.Example): SiteState = {
    this.copy(mainExample = Some(mainExample))
  }

  def getSettingWidget: SettingWidget[_] = {
    settingWidget.getOrElse(throw RuntimeException(s"[settingWidget] is undefined"))
  }

  def setSettingWidget(settingWidget: SettingWidget[_]): SiteState = {
    this.copy(settingWidget = Some(settingWidget))
  }
}