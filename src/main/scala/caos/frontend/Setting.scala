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

  private def getActive(path: String): List[String] = {
    def collectPaths(setting: Setting, path: List[String]): List[String] = {
      if (!setting.checked) Nil
      else if (setting.children.isEmpty) List(path.mkString("."))
      else setting.children.flatMap(child => collectPaths(child, path :+ child.name))
    }

    path2setting(path) match {
      case Some(setting) => collectPaths(setting, List.empty)
      case None => throw new RuntimeException(s"[$path] was not match on [$this]")
    }
  }

  private def renderFromPath(path: String): Boolean = {
    path2setting(path) match {
      case Some(setting) => setting.checked
      case _ => throw RuntimeException(s"[$path] was not matched on [$this]")
    }
  }

  private def path2setting(path: String): Option[Setting] = {
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
      case Nil => None
      case head :: tail =>
        if (this.name == head)
          resolvePath(this, tail)
        else
          None
    }
  }

  private def allFrom(setting: Setting): Set[Setting] = {
    if (setting.children.isEmpty)
      Set(setting)
    else
      Set(setting) ++ setting.children.flatMap(child => allFrom(child))
  }

  private def allLeavesFrom(setting: Setting): Set[Setting] = {
    allFrom(setting).filter(child => child.children.isEmpty)
  }

  private def allActiveFrom(setting: Setting): Set[Setting] = {
    allFrom(setting).filter(child => child.checked)
  }

  // @ telmo - could be optimized through a filter, but I like the compositional behaviour
  def allActiveLeavesFrom(setting: Setting): Set[Setting] = {
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

  def apply(path: String): List[String] = getActive(path)

  def unapply(setting: Setting): Option[(String, List[Setting], Boolean, List[String])] =
    Some((setting.name, setting.children, setting.checked, setting.options))
}