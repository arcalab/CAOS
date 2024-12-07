package caos.frontend

import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions

case class Setting(name: String, children: List[Setting] = List(), checked: Boolean = false, options: List[String] = List.empty) {
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
    val groupName = s"${this.name} ++ ${setting.name}"
    if (this.options.contains("allowAll")) {
      Setting(groupName, this.children :+ setting, this.checked, this.options)
    } else {
      Setting(groupName, List(this, setting), this.checked, List("allowAll"))
    }
  }

  override def toString: String = {
    def toStringAuxiliary(setting: Setting, ident: String = ""): String = {
      val currentString = s"$ident- ${setting.name} | ${setting.checked} | ${setting.options}\n "
      val childrenString = setting.children.map(child => toStringAuxiliary(child, ident + " ")).mkString
      currentString + childrenString
    }

    toStringAuxiliary(this)
  }

  def path2setting(path: String): Setting = {
    @tailrec
    def resolvePath(currentSetting: Setting, remainingPath: List[String]): Option[Setting] = {
      remainingPath match
        case Nil => Some(currentSetting)
        case head :: tail => currentSetting.children.find(_.name == head) match
          case Some(child) => resolvePath(child, tail)
          case None => None
    }

    path.split("\\.").toList match {
      case Nil => throw RuntimeException(s"could not resolve [$path]")
      case head :: tail if this.name == head =>
          resolvePath(this, tail) match
            case Some(setting) => setting
            case _ => throw RuntimeException(s"could not match [$path] to a Setting")
      case _ => throw RuntimeException(s"could not match [$path] to a Setting")
    }
  }

  def setChecked(path: String, value: Boolean): Setting = setChecked(path2setting(path), value)

  def setChecked(setting: Setting, value: Boolean): Setting = {
    Setting(
      this.name,
      this.children.map(_.setChecked(setting, value)),
      if (this == setting) value else this.checked,
      this.options
    )
  }

  def parentOf(child: Setting): Option[Setting] = {
    val parentsOf = Setting.allFromInclusive(this, setting => setting.children.contains(child))
    parentsOf.headOption
  }

  def allFromInclusive(path: String, filterCondition: Setting => Boolean = _ => true): Set[Setting] = Setting.allFromInclusive(path2setting(path), filterCondition)

  def allFrom(path: String, filterCondition: Setting => Boolean = _ => true): Set[Setting] = Setting.allFrom(path2setting(path), filterCondition)

  def allFromOrderedInclusive(path: String, filterCondition: Setting => Boolean = _ => true): List[Setting] = Setting.allFromOrderedInclusive(path2setting(path), filterCondition)

  def allFromOrdered(path: String, filterCondition: Setting => Boolean = _ => true): List[Setting] = Setting.allFromOrdered(path2setting(path), filterCondition)

  def allLeavesFrom(path: String): Set[Setting] = Setting.allLeavesFrom(path2setting(path))

  def allActiveFrom(path: String): Set[Setting] = Setting.allActiveFrom(path2setting(path))

  def allActiveLeavesFrom(path: String): Set[Setting] = Setting.allActiveLeavesFrom(path2setting(path))

  def apply(path: String): Set[Setting] = Setting.apply(path2setting(path))

  def unapply: Option[(String, List[Setting], Boolean, List[String])] = Some((this.name, this.children, this.checked, this.options))
}

object Setting {
  def allFromInclusive(setting: Setting, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    val filteredSetting = if (filterCondition(setting)) Set(setting) else Set.empty
    filteredSetting ++ setting.children.flatMap(child => allFromInclusive(child, filterCondition))
  }

  def allFrom(setting: Setting, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    setting.children.flatMap(child => allFromInclusive(child, filterCondition)).toSet
  }

  def allFromOrderedInclusive(setting: Setting, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    val filteredSetting = if (filterCondition(setting)) List(setting) else List.empty
    filteredSetting ++ setting.children.flatMap(child => allFromOrderedInclusive(child, filterCondition))
  }

  def allFromOrdered(setting: Setting, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    setting.children.flatMap(child => allFromOrderedInclusive(child, filterCondition))
  }

  def allLeavesFrom(setting: Setting): Set[Setting] = allFrom(setting, setting => setting.children.isEmpty)

  def allActiveFrom(setting: Setting): Set[Setting] = allFrom(setting, setting => setting.checked)

  def allActiveLeavesFrom(setting: Setting): Set[Setting] = allFrom(setting, setting => setting.children.isEmpty && setting.checked)

  def apply(setting: Setting): Set[Setting] = allActiveLeavesFrom(setting)

  def unapply(setting: Setting): Option[(String, List[Setting], Boolean, List[String])] = Some((setting.name, setting.children, setting.checked, setting.options))
}