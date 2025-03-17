package caos.frontend

import caos.frontend.Setting

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object SettingParser extends RegexParsers {
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  override def skipWhitespace: Boolean = true

  private def nameID = """[\w\-_ ()]+""".r
  private def optionID = """[\w\-_ ]+""".r

  private def bool: Parser[Boolean] = "true" ^^  (_ => true) | "false" ^^ (_ => false)

  private def maybeSetting: Parser[Setting] = {
    opt(setting) ^^ (settingOption => settingOption.getOrElse(Setting()))
  }

  private def setting: Parser[Setting] = {
    "Setting(" ~> nameID ~ "," ~ maybeChildren ~ "," ~ bool ~ "," ~ maybeOptions <~ ")" ^^ {
      case name ~ _ ~ children ~ _ ~ checked ~ _ ~ options =>
        Setting(name, children, checked, options)
    }
  }

  private def maybeChildren: Parser[List[Setting]] = {
    "List()" ^^ (_ => List.empty) | "List(" ~> repsep(setting, ",") <~ ")"
  }

  private def maybeOptions: Parser[List[String]] = {
    "List()" ^^ (_ => List.empty) | "List(" ~> repsep(optionID, ",") <~ ")"
  }

  def apply(input: String): Setting = {
    parseAll(maybeSetting, input) match
      case Success(setting, _) =>
        setting
      case failure: NoSuccess =>
        throw new RuntimeException(s"Parsing failed with msg=[${failure.msg}] and next=[${failure.next}]")
  }
}