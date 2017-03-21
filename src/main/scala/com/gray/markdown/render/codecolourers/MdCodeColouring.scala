package com.gray.markdown.render.codecolourers

import com.gray.markdown.util.MapOne
import com.gray.string.AttributedString
import com.gray.string.domain.{Attribute, Format}

import scala.io.AnsiColor
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

trait MdCodeColouring extends MapOne {

  protected val RED = Format(foreground = Some(AnsiColor.RED))
  protected val BLUE = Format(Some(AnsiColor.BLUE))
  protected val WHITE = Format(Some(AnsiColor.WHITE))
  protected val GREEN = Format(Some(AnsiColor.GREEN))

  protected val BOLD_RED = Format(foreground = Some(AnsiColor.RED), other = Some(List(AnsiColor.BOLD)))
  protected val BOLD_YELLOW = Format(foreground = Some(AnsiColor.YELLOW), other = Some(List(AnsiColor.BOLD)))
  protected val BOLD_BLUE = Format(foreground = Some(AnsiColor.BLUE), other = Some(List(AnsiColor.BOLD)))

  protected val COMMENTED = Format(foreground = Some(AnsiColor.MAGENTA))

  val COMMENT_REGEX_SLASH = """\/\/.*|\/\*(.|\n)*?\*\/""".r
  val COMMENT_REGEX_HASH = "#.*".r

  val QUOTED_REGEX = """\".*?(?<!\\)\"""".r

  protected def applyFormat(format: Format, regex: Regex, string: AttributedString) = {
    regex.findAllMatchIn(string.string).foldLeft(string)((s, m) => {
      s < Attribute(format, m.start, m.end)
    })
  }

  protected def applyFormat(format: Format, matches: Seq[Match], string: AttributedString) = {
    matches.foldLeft(string)((s, m) => {
      s < Attribute(format, m.start, m.end)
    })
  }

  protected def applyFormatIterating(format: Format, matches: Seq[Match], string: AttributedString)(f: Match => (Int, Int)) = {
    matches.foldLeft(string)((s, m) => {
      val range = f(m)
      s < Attribute(format, range._1, range._2)
    })
  }

  def applyFormatExcludingMatches(format: Format, matches: Seq[Match], string: AttributedString, excluding: Seq[Match]) = {
    val realMatches = matches.filter(m => {
      !excluding.exists(e => e.start <= m.start && e.end >= m.end)
    })
    applyFormat(format, realMatches, string)
  }
  def applyFormatExcludingMatches(format: Format, regex: Regex, string: AttributedString, excluding: Seq[Match]) = {
    val realMatches = regex.findAllMatchIn(string.string).toSeq.filter(m => {
      !excluding.exists(e => e.start <= m.start && e.end >= m.end)
    })
    applyFormat(format, realMatches, string)
  }

  protected def applyFormatConditionally(format: Format, matches: Seq[Match], string: AttributedString)(f: Match => Option[(Int, Int)]) = {
    matches.foldLeft(string)((s, m) => {
      f(m) match {
        case Some((start, end)) =>
          s < Attribute(format, start, end)
        case _ => s
      }
    })
  }

  def colourCode(string: AttributedString): AttributedString
}
