package com.gray.markdown.render.renderingrules

import com.gray.markdown.render.codecolourers._
import com.gray.markdown.{MdCode, MdLinkReference, MdParagraph}
import com.gray.string.AttributedString
import com.gray.string.domain.Format

import scala.io.AnsiColor

object MdCodeRenderer extends MdParagraphRenderer {

  private val WHITE_FOREGROUND = Format(Some(AnsiColor.WHITE))

  override def render(mdParagraph: MdParagraph,
                      width: Int,
                      linkRefs: List[MdLinkReference],
                      renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
                     ): Option[AttributedString] = mdParagraph match {
    case MdCode(string, language, _) =>
      val whiteString = AttributedString(string) << WHITE_FOREGROUND
      val codeString = colourCode(whiteString, language).wrapToWidth(width-2)
      colourBackground(codeString, width) | Some.apply
    case _ => None
  }

  def colourCode(string: AttributedString, languageOpt: Option[String]) = {
    languageOpt match {
      case Some(language) if codeColourers.contains(language) =>
        codeColourers(language).colourCode(string)
      case _ => string
    }
  }

  private val emptyCodeSpace = AttributedString(" ") << codeBackgroundFormat

  def colourBackground(attributedString: AttributedString, width: Int) = {
    val extraLine = AttributedString(concatenate(" ", width)) << codeBackgroundFormat
    val lines = attributedString.split("\n")
    val block = blockify(lines, width - 2).map(l => emptyCodeSpace + (l << codeBackgroundFormat) + emptyCodeSpace)
    (extraLine +: block :+ extraLine).reduce(_ + newLine + _)
  }

  val codeColourers: Map[String, MdCodeColouring] = Map(
    "json" -> MdJsonCodeColourer,
    "scala" -> MdScalaCodeColourer,
    "php" -> MdPhpCodeColourer,
    "sh" -> MdBashCodeColourer,
    "ss" -> MdBashCodeColourer
  )

}
