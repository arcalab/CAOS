package caos.frontend

import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions

case class Setting(name: String, children: List[Setting] = List(), var checked: Boolean = true, options: List[String] = List.empty) {
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

  private def path2setting(path: String): Setting = {
    @tailrec
    def resolvePath(currentSetting: Setting, remainingPath: List[String]): Option[Setting] = {
      remainingPath match
        case Nil => Some(currentSetting)
        case head :: tail =>
          currentSetting.children.find {
            _.name == head
          } match
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

  private def allFrom(path: String): Set[Setting] = allFrom(path2setting(path))

  private def allFrom(setting: Setting): Set[Setting] = {
    if (setting.children.isEmpty)
      Set(setting)
    else
      Set(setting) ++ setting.children.flatMap(child => allFrom(child))
  }

  private def allLeavesFrom(path: String): Set[Setting] = allLeavesFrom(path2setting(path))

  private def allLeavesFrom(setting: Setting): Set[Setting] = {
    allFrom(setting).filter(child => child.children.isEmpty)
  }

  private def allActiveFrom(path: String): Set[Setting] = allActiveFrom(path2setting(path))

  private def allActiveFrom(setting: Setting): Set[Setting] = {
    allFrom(setting).filter(child => child.checked)
  }

  private def allActiveLeavesFrom(path: String): Set[Setting] = allActiveLeavesFrom(path2setting(path))

  // @ telmo - could be optimized through ç a filter, but I like the compositional behaviour
  private def allActiveLeavesFrom(setting: Setting): Set[Setting] = {
    allLeavesFrom(setting).intersect(allActiveFrom(setting))
  }

  override def toString: String = {
    def toStringAuxiliary(setting: Setting, ident: String = ""): String = {
      val currentString = s"$ident- ${setting.name} ${setting.checked}\n "
      val childrenString = setting.children.map(child => toStringAuxiliary(child, ident + " ")).mkString
      currentString + childrenString
    }

    toStringAuxiliary(this)
  }

  def apply(path: String): Set[Setting] = apply(path2setting(path))

  def apply(setting: Setting): Set[Setting] = allActiveLeavesFrom(setting)

  def unapply(setting: Setting): Option[(String, List[Setting], Boolean, List[String])] =
    Some((setting.name, setting.children, setting.checked, setting.options))
}