package com.gray.markdown.render.codecolourers
import com.gray.string.AttributedString
import com.gray.string.domain.Format

import scala.io.AnsiColor

object MdScalaCodeColourer extends MdCodeColouring {

  val tripleQuoteRegex = """s?\"\"\"(.|\n)*?(?<!\\)\"\"\"""".r
  val regularStringRegex = """s?\".*?(?<!\\)\"""".r

  val protectedWords = """\b(def|val|var|for|if|do|match|class|case|object|this|package|trait|sealed|final|abstract|protected|import)\b""".r

  val punctuation = """\.|;|,""".r

  val defsRegex = """(?<=def ) *[a-zA-Z][a-zA-z0-9_\-+*]*""".r

  override def colourCode(string: AttributedString): AttributedString = {
    val tripleQuoteMatches = tripleQuoteRegex.findAllMatchIn(string.string).toList
    val singleQuoteMatches = regularStringRegex.findAllMatchIn(string.string).toList
    val allQuoteMatches = tripleQuoteMatches ++ singleQuoteMatches
    val commentedMatches = COMMENT_REGEX_SLASH.findAllMatchIn(string.string).toSeq

    val nonCode = allQuoteMatches ++ commentedMatches

    applyFormat(COMMENTED, commentedMatches, string) |
      (applyFormatExcludingMatches(GREEN, tripleQuoteMatches, _, commentedMatches)) |
      (applyFormatExcludingMatches(GREEN, singleQuoteMatches, _, tripleQuoteMatches ++ commentedMatches)) |
      (applyFormatExcludingMatches(BOLD_RED, protectedWords, _, nonCode)) |
      (applyFormatExcludingMatches(BOLD_YELLOW, defsRegex, _, nonCode)) |
      (applyFormatExcludingMatches(MAGENTA, punctuation, _, nonCode)) |
      (applyFormatExcludingMatches(Format(foreground = Some(AnsiColor.CYAN)), "\\b\\d+\\b".r, _, nonCode))
  }

}
