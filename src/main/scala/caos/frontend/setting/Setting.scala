package caos.frontend.setting

import scala.annotation.targetName

case class Setting(
                    name: String = "",
                    children: Set[Setting] = Set.empty,
                    checked: Boolean = false,
                    options: Set[String] = Set.empty
                  ):
  /**
   * creates a new Setting instance where at most one child can be checked at the time
   *
   * in addition to the Setting passed as a parameter,
   * the new instance fathers the current instance's children or the current instance itself,
   * depending on whether the current instance is already '''allowOne''' or not
   *
   * @param settingB another Setting instance to be made available as a choice
   * @return the Setting instance where at most one child can be checked at the time
   */
  @targetName("allowOne")
  def ||(settingB: Setting): Setting =
    val groupName = s"${this.name}||${settingB.name}"
    if this.options.contains("allowOne")
    then
      Setting(groupName, this.children + settingB, this.checked, this.options)
    else
      Setting(groupName, Set(this, settingB), this.checked, Set("allowOne"))
  end ||

  /**
   * creates a new Setting instance where any number of children be checked at the time
   *
   * in addition to the Setting passed as a parameter,
   * the new instance fathers the current instance's children or the current instance itself,
   * depending on whether the current instance is already '''allowAll''' or not
   *
   * @param settingB another Setting instance to be made available as a choice
   * @return the Setting instance where any number of children can be checked at the time
   */
  @targetName("allowAll")
  def &&(settingB: Setting): Setting =
    val groupName = s"${this.name}&&${settingB.name}"
    if this.options.contains("allowAll")
    then
      Setting(groupName, this.children + settingB, this.checked, this.options)
    else
      Setting(groupName, Set(this, settingB), this.checked, Set("allowAll"))
  end &&
end Setting