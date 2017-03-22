package com.gray.markdown.render.codecolourers
import com.gray.string.AttributedString

object MdPhpCodeColourer extends MdCodeColouring {

  val protectedWords =
    """\b(public|private|function|class|extends|return|for|foreach|as|if|else|break|case|switch)\b|;|,""".r
  val variables = """\$\w+""".r

  override def colourCode(string: AttributedString): AttributedString = {
    val plainString = string.string
    val comments =
    COMMENT_REGEX_HASH.findAllMatchIn(plainString).toSeq ++ COMMENT_REGEX_SLASH.findAllMatchIn(plainString)
    val strings =
    QUOTED_REGEX.findAllMatchIn(plainString).toSeq ++ QUOTED_SINGLE_REGEX.findAllMatchIn(plainString)

    val classNameRegex = """(?<=class )\s*\w+""".r
    val functionNameRegex = """(?<=function )\s*\w+""".r
    val functionInlineRegex = """(?:(?<=->)|(?<=::))\w+ {0,1}(?=\()""".r

    val nonCode = comments ++ strings

    applyFormat(COMMENTED, comments, string) |
      (applyFormatExcludingMatches(GREEN, strings, _, comments)) |
      (applyFormatExcludingMatches(BOLD_RED, protectedWords, _, comments)) |
      (applyFormatExcludingMatches(BOLD_BLUE, variables.findAllMatchIn(plainString).toSeq, _, nonCode)) |
      (applyFormatExcludingMatches(RED, "<\\?(php)?".r, _, nonCode)) |
      (applyFormatExcludingMatches(BOLD_YELLOW, classNameRegex, _, nonCode)) |
      (applyFormatExcludingMatches(BOLD_YELLOW, functionNameRegex, _, nonCode)) |
      (applyFormatExcludingMatches(BOLD_YELLOW, functionInlineRegex, _, nonCode)) |
      (applyFormatExcludingMatches(RED, "\\?>".r, _, nonCode))
  }
}
