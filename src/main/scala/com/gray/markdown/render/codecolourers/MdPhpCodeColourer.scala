package com.gray.markdown.render.codecolourers
import com.gray.string.AttributedString

object MdPhpCodeColourer extends MdCodeColouring {

  val protectedWords = """\b(public|private|function|class|return|if|else|break|case|switch|;|,)\b""".r
  val variables = """\$\w+""".r

  override def colourCode(string: AttributedString): AttributedString = {
    val plainString = string.string
    val commented = (COMMENT_REGEX_HASH.findAllMatchIn(plainString) ++
      COMMENT_REGEX_SLASH.findAllMatchIn(plainString)).toSeq
    val strings = QUOTED_REGEX.findAllMatchIn(plainString).toSeq

    val nonCode = commented ++ strings

    applyFormat(COMMENTED, commented, string) |
      (applyFormatExcludingMatches(GREEN, strings, _, commented)) |
      (applyFormatExcludingMatches(BOLD_RED, protectedWords, _, commented)) |
      (applyFormatExcludingMatches(BOLD_BLUE, variables.findAllMatchIn(plainString).toSeq, _, nonCode)) |
      (applyFormatExcludingMatches(RED, "<\\?(php)?".r, _, nonCode)) |
      (applyFormatExcludingMatches(RED, "\\?>".r, _, nonCode))
  }
}
