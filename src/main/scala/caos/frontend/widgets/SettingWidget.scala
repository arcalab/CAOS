package caos.frontend.widgets

import org.scalajs.dom.{document, html, Event}

import caos.frontend.{Configurator, Documentation, Setting}


abstract class SettingWidget[A](title: String, doc: Documentation, config: Configurator[A]) extends Widget[Setting](title, doc):
  protected var setting: Setting = config.setting

  protected val buttons: List[(Either[String, String], (() => Unit, String))]

  def reload(): Unit

  override def init(div: Block, visible: Boolean): Unit =
    panelBox(div, visible, buttons = buttons)
      .append("div")
      .attr("id", "setting-container")

    update()
  end init

  override def get: Setting = setting

  def set(setting: Setting): Unit = this.setting = setting

  override def update(): Unit =
    val settingContainerDiv = document.getElementById("setting-container").asInstanceOf[html.Div]
    settingContainerDiv.innerHTML = ""

    renderSetting(setting, settingContainerDiv)
  end update

  private def renderSetting(currentSetting: Setting, parentDiv: html.Div, indentationLevel: Int = 0): Unit =
    val currentDiv = document.createElement("div").asInstanceOf[html.Div]
    currentDiv.setAttribute("class", "setting-div") // ns
    currentDiv.style.paddingLeft = s"${indentationLevel * 20}px"

    val title = document.createElement("h4").asInstanceOf[html.Heading]
    title.textContent = s"${currentSetting.name}"
    currentDiv.appendChild(title)

    val checkbox = document.createElement("input").asInstanceOf[html.Input]
    checkbox.setAttribute("type", "checkbox")
    checkbox.setAttribute("name", currentSetting.name)
    checkbox.checked = currentSetting.checked

    checkbox.onchange = (_: Event) => {
      val isChecked = checkbox.checked

      // @ telmo - the logic here is quite tricky - this seems simple but this order is almost mandatory
      setting.parentOf(currentSetting) match
        case Some(parentSetting) if parentSetting.options.contains("allowOne") && isChecked =>
          parentSetting.children.foreach(childSetting =>
            if (childSetting != currentSetting) setting = setting.setChecked(childSetting, false)
            setting = setting.setChecked(currentSetting, isChecked)
          )
        case Some(_) =>
          setting = setting.setChecked(currentSetting, isChecked)
          if (!isChecked) Setting.allFromOrdered(currentSetting).foreach(child => setting = setting.setChecked(child, isChecked))
        case None =>
          setting = setting.setChecked(currentSetting, isChecked)

      val settingContainerDiv = document.getElementById("setting-container").asInstanceOf[html.Div]
      settingContainerDiv.innerHTML = ""
      renderSetting(setting, settingContainerDiv)
    }

    currentDiv.appendChild(checkbox)

    parentDiv.appendChild(currentDiv)

    if (currentSetting.children.nonEmpty) {
      val childrenContainerDiv = document.createElement("div").asInstanceOf[html.Div]
      childrenContainerDiv.setAttribute("class", "children-container") // ns
      currentSetting.children.foreach(childSetting => renderSetting(childSetting, childrenContainerDiv, indentationLevel + 1))
      currentDiv.appendChild(childrenContainerDiv)
    }
  end renderSetting

end SettingWidget
