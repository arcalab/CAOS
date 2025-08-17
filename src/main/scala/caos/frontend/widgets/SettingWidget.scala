package caos.frontend.widgets

import org.scalajs.dom.{Event, document, html}
import caos.frontend.{Configurator, Documentation, Setting}


abstract class SettingWidget[A](title: String, doc: Documentation, config: Configurator[A]) extends Widget[Setting](title, doc):
  protected var setting: Option[Setting] = Some(config.setting)

  protected val buttons: List[(Either[String, String], (() => Unit, String))]

  def reload(): Unit

  override def init(div: Block, visible: Boolean): Unit =
    panelBox(div, visible, buttons = buttons)
      .append("div")
      .attr("id", "setting-container")

    update()
  end init

  override def get: Setting = setting.get

  def set(setting: Setting): Unit = this.setting = Some(setting)

  override def update(): Unit =
    val settingContainerDiv = document.getElementById("setting-container").asInstanceOf[html.Div]
    settingContainerDiv.innerHTML = ""

    if setting.isDefined then renderSetting(setting.get, settingContainerDiv, currentPath = setting.get.name)
  end update

  private def renderSetting(currentSetting: Setting, parentDiv: html.Div, indentationLevel: Int = 0, currentPath: String): Unit =
    val currentSuperDiv = document.createElement("div").asInstanceOf[html.Div]
    currentSuperDiv.style.paddingLeft = s"${indentationLevel * 10}px"

    val currentDiv = document.createElement("div").asInstanceOf[html.Div]
    currentDiv.setAttribute("class", "setting-container")
    currentDiv.style.display = "flex"
    currentDiv.style.columnGap = "10px"

    val title = document.createElement("h4").asInstanceOf[html.Heading]
    title.textContent = s"${currentSetting.name}"
    title.style.margin = "0"
    title.style.fontFamily = "monospace"
    title.style.fontSize   = "15px"

    val checkbox = document.createElement("input").asInstanceOf[html.Input]
    checkbox.setAttribute("type", "checkbox")
    checkbox.setAttribute("name", currentSetting.name)
    checkbox.checked = currentSetting.checked
    checkbox.onchange = (_: Event) => {
      val isChecked = checkbox.checked

      setting.get.parentOf(currentPath) match
        case Some(parentSetting) if parentSetting.options.contains("allowOne") && isChecked =>
          parentSetting.children.foreach(childSetting => setting =
            val parentPath = currentPath.reverse.replaceFirst(s".${currentSetting.name}".reverse, "").reverse
            Some(setting.get.withChecked(s"$parentPath.${childSetting.name}", false)))
          setting = Some(setting.get.checkUpstream(currentPath, true))
        case _ if isChecked =>
          setting = Some(setting.get.checkUpstream(currentPath, true))
        case _ =>
          setting = Some(setting.get.checkDownstream(currentPath, false))
      setting = Some(setting.get.withChecked(currentPath, isChecked))

      val settingContainerDiv = document.getElementById("setting-container").asInstanceOf[html.Div]
      settingContainerDiv.innerHTML = ""
      renderSetting(setting.get, settingContainerDiv, currentPath = setting.get.name)
    }

    currentDiv.appendChild(checkbox)
    currentDiv.appendChild(title)

    if indentationLevel > 0 then currentSuperDiv.appendChild(currentDiv) // avoids rendering the initial root
    parentDiv.appendChild(currentSuperDiv)

    if (currentSetting.children.nonEmpty) {
      val childrenContainerDiv = document.createElement("div").asInstanceOf[html.Div]
      currentSetting.children.foreach(childSetting => renderSetting(childSetting, childrenContainerDiv, indentationLevel + 1, s"$currentPath.${childSetting.name}"))
      currentSuperDiv.appendChild(childrenContainerDiv)
    }
  end renderSetting
end SettingWidget
