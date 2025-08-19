package caos.frontend.setting

import scala.annotation.targetName

case class Setting(name: String = "", children: List[Setting] = List.empty, checked: Boolean = false, options: List[String] = List.empty) {
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

  /** Yields the parent of the '''Setting''' described by '''path''', provided that '''this''' is the root of the tree */
  def parentOf(path: String): Option[Setting] = {
    resolvePath(path) match
      case Some(setting) => parentOfAuxiliary(this, setting)
      case _ => None
  }

  private def parentOfAuxiliary(root: Setting, child: Setting): Option[Setting] = {
    allFromAuxiliary(root, _.children.exists(_ eq child)).headOption
  }

  /** Yields a new '''Setting''' tree with '''path''' set to '''value''' */
  def withChecked(path: String, value: Boolean): Setting = {
    resolvePath(path).map(withChecked(_, value)).get
  }

  private def withChecked(setting: Setting, value: Boolean): Setting = {
    if (this eq setting) {
      this.copy(checked = value)
    } else {
      this.copy(children = this.children.map(_.withChecked(setting, value)))
    }
  }

  /** Yields a new '''Setting''' tree with all members of '''path''' (inclusive) set to '''value''' */
  def checkAll(path: String, value: Boolean): Setting = {
    checkUpstream(path, value).withChecked(resolvePath(path).get, value)
  }

  /** Yields a new '''Setting''' tree with all members of '''path''' (exclusive) set to '''value''' */
  def checkUpstream(path: String, value: Boolean): Setting = {
    resolvePath(path).map(checkUpstreamAuxiliary(_, value)).get
  }

  private def checkUpstreamAuxiliary(setting: Setting, value: Boolean): Setting = {
    parentOfAuxiliary(this, setting) match
      case Some(parentSetting) =>
        val updatedParent = checkUpstreamAuxiliary(parentSetting, value)
        updatedParent.withChecked(parentSetting, value)
      case None =>
        this
  }

  /** Yields a new '''Setting''' tree with all members following '''path''' (exclusive) set to '''value''' */
  def checkDownstream(path: String, value: Boolean): Setting = {
    resolvePath(path).map(checkDownstreamAuxiliary(_, value)).getOrElse(this)
  }

  private def checkDownstreamAuxiliary(setting: Setting, value: Boolean): Setting = {
    if (this eq setting) {
      this.copy(children = this.children.map(_.withCheckedDownstream(value)))
    } else {
      this.copy(children = this.children.map(_.checkDownstreamAuxiliary(setting, value)))
    }
  }

  private def withCheckedDownstream(value: Boolean): Setting = {
    this.copy(checked = value, children = this.children.map(_.withCheckedDownstream(value)))
  }

  /** Yields the set of '''Setting'''s following '''path''' (inclusive) that hold for '''predicate''' */
  def allFrom(path: String, predicate: Setting => Boolean = _ => true): Set[Setting] = {
    resolvePath(path).map(allFromAuxiliary(_, predicate)).getOrElse(Set.empty)
  }

  private def allFromAuxiliary(setting: Setting, predicate: Setting => Boolean): Set[Setting] = {
    (if predicate(setting) then Set(setting) else Set.empty) ++ setting.children.flatMap(allFromAuxiliary(_, predicate))
  }

  /** Yields the set of '''Setting'''s following '''path''' (inclusive) that are both leaves and checked */
  def allActiveLeavesFrom(path: String): Set[Setting] = {
    resolvePath(path).map(allFromAuxiliary(_, setting => setting.checked && setting.children.isEmpty)).getOrElse(Set.empty)
  }

  private def resolvePath(path: String): Option[Setting] = {
    resolvePathAuxiliary(path.split("\\.").toList)
  }

  private def resolvePathAuxiliary(remainingPath: List[String]): Option[Setting] = {
    remainingPath match
      case Nil => None
      case this.name :: Nil => Some(this)
      case this.name :: tail => this.children.collectFirst(Function.unlift(_.resolvePathAuxiliary(tail)))
      case head :: tail => None
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
}