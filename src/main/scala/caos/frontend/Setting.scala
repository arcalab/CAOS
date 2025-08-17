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

  override def toString: String = {
    toStringAuxiliary()
  }

  private def toStringAuxiliary(ident: String = ""): String = {
    val childrenString = this.children.map(_.toStringAuxiliary(ident + " ")).mkString
    s"$ident- ${this.name} | ${this.checked} | ${this.options}\n$childrenString"
  }

  def toStringRaw: String = {
    s"Setting($name, ${children.map(child => child.toStringRaw)}, $checked, $options)"
  }

  def resolvePath(path: String): Option[Setting] = {
    resolvePathAuxiliary(path.split("\\.").toList)
  }

  private def resolvePathAuxiliary(remainingPath: List[String]): Option[Setting] = {
    remainingPath match
      case Nil => None
      case this.name :: Nil => Some(this)
      case this.name :: tail => this.children.collectFirst(Function.unlift(_.resolvePathAuxiliary(tail)))
      case head :: tail => None
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
      case Some(setting) => parentOf(this, setting)
      case _ => None
  }

  private def parentOf(root: Setting, child: Setting): Option[Setting] = {
    allFromAuxiliary(root, _.children.exists(_ eq child)).headOption
  }

  def checkAll(path: String, value: Boolean): Setting = {
    checkUpstream(path, value).setChecked(path, value)
  }

  def checkUpstream(path: String, value: Boolean): Setting = {
    resolvePath(path).map(checkUpstreamAuxiliary(_, value)).getOrElse(this)
  }

  private def checkUpstreamAuxiliary(setting: Setting, value: Boolean): Setting = {
    parentOf(this, setting) match
      case Some(parentSetting) =>
        val updatedParent = checkUpstreamAuxiliary(parentSetting, value)
        updatedParent.setChecked(parentSetting, value)
      case None =>
        this
  }

  def checkDownstream(path: String, value: Boolean): Setting = {
    resolvePath(path).map(checkDownstreamAuxiliary(_, value)).getOrElse(this)
  }

  private def checkDownstreamAuxiliary(setting: Setting, value: Boolean): Setting = {
    if (this eq setting) {
      this.copy(children = this.children.map(_.setCheckDownstream(value)))
    } else {
      this.copy(children = this.children.map(_.checkDownstreamAuxiliary(setting, value)))
    }
  }

  private def setCheckDownstream(value: Boolean): Setting = {
    this.copy(checked = value, children = this.children.map(_.setCheckDownstream(value)))
  }

  def allFrom(path: String, predicate: Setting => Boolean = _ => true): Set[Setting] = {
    resolvePath(path).map(allFromAuxiliary(_, predicate)).getOrElse(Set.empty)
  }

  private def allFromAuxiliary(setting: Setting, predicate: Setting => Boolean): Set[Setting] = {
    (if predicate(setting) then Set(setting) else Set.empty) ++ setting.children.flatMap(allFromAuxiliary(_, predicate))
  }

  def allActiveLeavesFrom(path: String): Set[Setting] = {
    resolvePath(path).map(allFromAuxiliary(_, setting => setting.checked && setting.children.isEmpty)).getOrElse(Set.empty)
  }

  def apply(path: String): Set[Setting] = {
    allActiveLeavesFrom(path)
  }

  def unapply: Option[(String, List[Setting], Boolean, List[String])] = {
    Some(this.name, this.children, this.checked, this.options)
  }
}