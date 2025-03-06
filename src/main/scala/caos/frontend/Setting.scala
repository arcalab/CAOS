package caos.frontend

import scala.annotation.targetName
import scala.language.implicitConversions

case class Setting(name: String = null, children: List[Setting] = List(), var checked: Boolean = false, options: List[String] = List.empty) {
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

  def deepCopy: Setting = {
    this.copy(children = this.children.map(_.deepCopy))
  }

  private def toStringAuxiliary(ident: String = ""): String = {
    val childrenString = this.children.map(_.toStringAuxiliary(ident + " ")).mkString
    s"$ident- ${this.name} | ${this.checked} | ${this.options}\n$childrenString"
  }

  override def toString: String = {
    toStringAuxiliary()
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

  def setChecked(setting: Setting, value: Boolean): Setting = {
    if (this eq setting) {
      this.checked = value
    } else {
      this.children.map(_.setChecked(setting, value))
    }
    this
  }

  def parentOf(child: Setting): Option[Setting] = {
    Setting.allFromInclusive(this, _.children.contains(child)).headOption
  }

  def setCheckedPath(path: String, value: Boolean): Setting = {
    resolvePath(path) match
      case Some(setting) => setCheckedPath(setting, value)
      case None => this
  }

  def setCheckedPath(setting: Setting, value: Boolean): Setting = {
    setCheckedUpstream(setting, value)
    setChecked(setting, value)
  }

  def setCheckedUpstream(setting: Setting, value: Boolean): Setting = {
    parentOf(setting) match
      case Some(parentSetting) =>
        setCheckedUpstream(parentSetting, value)
        setChecked(parentSetting, value)
      case None =>
        this
  }

  def setCheckedDownstream(setting: Setting, value: Boolean): Setting = {
    setting.children.map( childSetting =>
      setCheckedDownstream(childSetting, value)
      setChecked(childSetting, value)
    )
    this
  }

  def allFromInclusive(path: String, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allFromInclusive(setting, filterCondition)
      case None => Set.empty
  }

  def allFrom(path: String, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allFrom(setting, filterCondition)
      case None => Set.empty
  }

  def allFromOrderedInclusive(path: String, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allFromOrderedInclusive(setting, filterCondition)
      case None => List.empty
  }

  def allFromOrdered(path: String, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allFromOrdered(setting, filterCondition)
      case None => List.empty
  }

  def allLeavesFrom(path: String): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allLeavesFrom(setting)
      case None => Set.empty
  }

  def allActiveFrom(path: String): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allActiveFrom(setting)
      case None => Set.empty
  }

  def allActiveLeavesFrom(path: String): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allActiveLeavesFrom(setting)
      case None => Set.empty
  }

  def apply(path: String): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.apply(setting)
      case None => Set.empty
  }

  def unapply: Option[(String, List[Setting], Boolean, List[String])] = Some((this.name, this.children, this.checked, this.options))
}

object Setting {
  def allFromInclusive(setting: Setting, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    val filteredSetting = if (filterCondition(setting)) Set(setting) else Set.empty
    filteredSetting ++ setting.children.flatMap(allFromInclusive(_, filterCondition))
  }

  def allFrom(setting: Setting, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    setting.children.flatMap(allFromInclusive(_, filterCondition)).toSet
  }

  def allFromOrderedInclusive(setting: Setting, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    val filteredSetting = if (filterCondition(setting)) List(setting) else List.empty
    filteredSetting ++ setting.children.flatMap(allFromOrderedInclusive(_, filterCondition))
  }

  def allFromOrdered(setting: Setting, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    setting.children.flatMap(allFromOrderedInclusive(_, filterCondition))
  }

  def allLeavesFrom(setting: Setting): Set[Setting] = {
    allFrom(setting, _.children.isEmpty)
  }

  def allActiveFrom(setting: Setting): Set[Setting] = {
    allFrom(setting, _.checked)
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