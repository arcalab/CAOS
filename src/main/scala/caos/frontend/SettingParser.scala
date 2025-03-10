package caos.frontend

import caos.frontend.Setting

import scala.util.matching.Regex


object SettingParser {
  private val SettingRegex: Regex =
    """Setting\(([^,]+),\s*List\((.*)\),\s*(true|false),\s*List\(([^)]*)\)\)""".r

  def parseSetting(input: String): Setting = {
    def parseChildren(childrenStr: String): List[Setting] = {
      if (childrenStr.trim.isEmpty) List()
      else {
        val children = splitTopLevel(childrenStr)
        children.map(parseSetting)
      }
    }

    input match {
      case SettingRegex(name, childrenStr, checked, optionsStr) =>
        val children = parseChildren(childrenStr)
        val options = if (optionsStr.trim.isEmpty) List() else optionsStr.split(",").map(_.trim).toList
        Setting(name.trim, children, checked.toBoolean, options)
      case "Setting()" =>
        Setting()
      case _ =>
        throw new IllegalArgumentException(s"Invalid Setting format: $input")
    }
  }


  private def splitTopLevel(input: String): List[String] = {
    var depth = 0
    val buffer = new StringBuilder
    val result = scala.collection.mutable.ListBuffer[String]()

    input.foreach {
      case '(' =>
        depth += 1
        buffer.append('(')
      case ')' =>
        depth -= 1
        buffer.append(')')
      case ',' if depth == 0 =>
        result.append(buffer.toString().trim)
        buffer.clear()
      case c =>
        buffer.append(c)
    }

    if (buffer.nonEmpty) result.append(buffer.toString().trim)
    result.toList.filter(_.nonEmpty)
  }
}