package caos.frontend

import scala.annotation.targetName
import scala.language.implicitConversions

case class Setting(name: String = null, children: List[Setting] = List.empty, checked: Boolean = false, options: List[String] = List.empty) {
  @targetName("allowOne")
  def ||(setting: Setting): Setting = {
    val groupName = s"${this.name} || ${setting.name}"
    if (this.options.contains("allowOne")) {
      Setting(groupName, this.children :+ setting, this.checked, this.options)
    } else {
      Setting(groupName, List(this, setting), this.checked, List("allowOne"))
    }
  }

  @targetName("allowAll")
  def &&(setting: Setting): Setting = {
    val groupName = s"${this.name} && ${setting.name}"
    if (this.options.contains("allowAll")) {
      Setting(groupName, this.children :+ setting, this.checked, this.options)
    } else {
      Setting(groupName, List(this, setting), this.checked, List("allowAll"))
    }
  }

  private def toStringAuxiliary(ident: String = ""): String = {
    val childrenString = this.children.map(_.toStringAuxiliary(ident + " ")).mkString
    s"$ident- ${this.name} | ${this.checked} | ${this.options}\n$childrenString"
  }

  override def toString: String = {
    toStringAuxiliary()
  }

  def toStringRaw: String = {
    s"Setting($name, ${children.map(child => child.toStringRaw)}, $checked, $options)"
  }

  private def resolvePathAuxiliary(remainingPath: List[String]): Option[Setting] = {
    remainingPath match
      case Nil => None
      case this.name :: Nil  => Some(this)
      case this.name :: tail => this.children.collectFirst(Function.unlift(_.resolvePathAuxiliary(tail)))
      case head :: tail => None
  }

  def resolvePath(path: String): Option[Setting] = {
    resolvePathAuxiliary(path.split("\\.").toList)
  }

  def setChecked(path: String, value: Boolean): Setting = {
    resolvePath(path).map(setChecked(_, value)).getOrElse(this)
  }

  def setChecked(setting: Setting, value: Boolean): Setting = {
    if (this eq setting) {
      this.copy(checked = value)
    } else {
      this.copy(children = this.children.map(_.setChecked(setting, value)))
    }
  }

  def parentOf(path: String): Option[Setting] = {
    resolvePath(path) match
      case Some(setting) => parentOf(setting)
      case _ => None
  }

  def parentOf(child: Setting): Option[Setting] = {
    Setting.allFromInclusive(this, _.children.exists(_ eq child)).headOption
  }

  def setCheckedPath(path: String, value: Boolean): Setting = {
    setCheckedUpstream(path, value).setChecked(path, value)
  }

  def setCheckedUpstream(path: String, value: Boolean): Setting = {
    resolvePath(path).map(setCheckedUpstream(_, value)).getOrElse(this)
  }

  def setCheckedUpstream(setting: Setting, value: Boolean): Setting = {
    parentOf(setting) match
      case Some(parentSetting) =>
        val updatedParent = setCheckedUpstream(parentSetting, value)
        updatedParent.setChecked(parentSetting, value)
      case None =>
        this
  }

  def setCheckedDownstream(path: String, value: Boolean): Setting = {
    resolvePath(path).map(setCheckedDownstream(_, value)).getOrElse(this)
  }

  private def setCheckedDownstreamAuxiliary(value: Boolean): Setting = {
    this.copy(checked = value, children = this.children.map(_.setCheckedDownstreamAuxiliary(value)))
  }

  def setCheckedDownstream(setting: Setting, value: Boolean): Setting = {
    if (this eq setting) {
      this.copy(children = this.children.map(_.setCheckedDownstreamAuxiliary(value)))
    } else {
      this.copy(children = this.children.map(_.setCheckedDownstream(setting, value)))
    }
  }

  def allFrom(path: String, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    resolvePath(path).map(Setting.allFrom(_, filterCondition)).getOrElse(Set.empty)
  }

  def allActiveLeavesFrom(path: String): Set[Setting] = {
    resolvePath(path).map(Setting.allActiveLeavesFrom).getOrElse(Set.empty)
  }

  def apply(path: String): Set[Setting] = {
    resolvePath(path).map(Setting.apply).getOrElse(Set.empty)
  }

  def unapply: Option[(String, List[Setting], Boolean, List[String])] = {
    Setting.unapply(this)
  }
}

object Setting {
  def allFromInclusive(setting: Setting, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    val filteredSetting = if (filterCondition(setting)) Set(setting) else Set.empty
    filteredSetting ++ setting.children.flatMap(allFromInclusive(_, filterCondition))
  }

  def allFrom(setting: Setting, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    setting.children.flatMap(allFromInclusive(_, filterCondition)).toSet
  }

  def allActiveLeavesFrom(setting: Setting): Set[Setting] = {
    allFrom(setting, setting => setting.children.isEmpty && setting.checked)
  }

  def apply(setting: Setting): Set[Setting] = {
    allActiveLeavesFrom(setting)
  }

  def unapply(setting: Setting): Option[(String, List[Setting], Boolean, List[String])] = {
    Some((setting.name, setting.children, setting.checked, setting.options))
  }
}