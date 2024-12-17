package caos.frontend

import scala.annotation.targetName
import scala.language.implicitConversions

case class Setting(name: String = null, children: List[Setting] = List(), checked: Boolean = false, options: List[String] = List.empty) {
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

  //noinspection ScalaWeakerAccess
  def toString(ident: String = ""): String = {
    val childrenString = this.children.map(_.toString(ident + " ")).mkString
    s"$ident- ${this.name} | ${this.checked} | ${this.options}\n$childrenString"
  }

  //noinspection ScalaWeakerAccess
  private def resolvePathAuxiliary(remainingPath: List[String]): Option[Setting] = {
    remainingPath match
      case Nil => None
      case this.name :: Nil  => Some(this)
      case this.name :: tail => this.children.find(_.resolvePathAuxiliary(tail).isDefined)
      case head :: tail => None
  }

  //noinspection ScalaWeakerAccess
  def resolvePath(path: String): Option[Setting] = resolvePathAuxiliary(path.split("\\.").toList)

  //noinspection ScalaWeakerAccess
  def getChecked(path: String): Option[Boolean] = resolvePath(path).map(_.checked)

  //noinspection ScalaWeakerAccess
  def setChecked(path: String, value: Boolean): Setting = {
    resolvePath(path) match
      case Some(setting) => setChecked(setting, value)
      case None => this
  }

  //noinspection ScalaWeakerAccess
  def setChecked(setting: Setting, value: Boolean): Setting = {
    Setting(
      this.name,
      this.children.map(_.setChecked(setting, value)),
      if (this == setting) value else this.checked,
      this.options
    )
  }

  //noinspection ScalaWeakerAccess
  def parentOf(child: Setting): Option[Setting] = Setting.allFromInclusive(this, _.children.contains(child)).headOption

  //noinspection ScalaWeakerAccess
  def allFromInclusive(path: String, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allFromInclusive(setting, filterCondition)
      case None => Set.empty
  }

  //noinspection ScalaWeakerAccess
  def allFrom(path: String, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allFrom(setting, filterCondition)
      case None => Set.empty
  }

  //noinspection ScalaWeakerAccess
  def allFromOrderedInclusive(path: String, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allFromOrderedInclusive(setting, filterCondition)
      case None => List.empty
  }

  //noinspection ScalaWeakerAccess
  def allFromOrdered(path: String, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allFromOrdered(setting, filterCondition)
      case None => List.empty
  }

  //noinspection ScalaWeakerAccess
  def allLeavesFrom(path: String): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allLeavesFrom(setting)
      case None => Set.empty
  }

  //noinspection ScalaWeakerAccess
  def allActiveFrom(path: String): Set[Setting] = {
    resolvePath(path) match
      case Some(setting) => Setting.allActiveFrom(setting)
      case None => Set.empty
  }

  //noinspection ScalaWeakerAccess
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
  //noinspection ScalaWeakerAccess
  def allFromInclusive(setting: Setting, filterCondition: Setting => Boolean = _ => true): Set[Setting] = {
    val filteredSetting = if (filterCondition(setting)) Set(setting) else Set.empty
    filteredSetting ++ setting.children.flatMap(allFromInclusive(_, filterCondition))
  }

  //noinspection ScalaWeakerAccess
  def allFrom(setting: Setting, filterCondition: Setting => Boolean = _ => true): Set[Setting] = setting.children.flatMap(allFromInclusive(_, filterCondition)).toSet

  //noinspection ScalaWeakerAccess
  def allFromOrderedInclusive(setting: Setting, filterCondition: Setting => Boolean = _ => true): List[Setting] = {
    val filteredSetting = if (filterCondition(setting)) List(setting) else List.empty
    filteredSetting ++ setting.children.flatMap(allFromOrderedInclusive(_, filterCondition))
  }

  //noinspection ScalaWeakerAccess
  def allFromOrdered(setting: Setting, filterCondition: Setting => Boolean = _ => true): List[Setting] = setting.children.flatMap(allFromOrderedInclusive(_, filterCondition))

  //noinspection ScalaWeakerAccess
  def allLeavesFrom(setting: Setting): Set[Setting] = allFrom(setting, _.children.isEmpty)

  //noinspection ScalaWeakerAccess
  def allActiveFrom(setting: Setting): Set[Setting] = allFrom(setting, _.checked)

  //noinspection ScalaWeakerAccess
  def allActiveLeavesFrom(setting: Setting): Set[Setting] = allFrom(setting, setting => setting.children.isEmpty && setting.checked)

  def apply(setting: Setting): Set[Setting] = allActiveLeavesFrom(setting)

  def unapply(setting: Setting): Option[(String, List[Setting], Boolean, List[String])] = Some((setting.name, setting.children, setting.checked, setting.options))
}