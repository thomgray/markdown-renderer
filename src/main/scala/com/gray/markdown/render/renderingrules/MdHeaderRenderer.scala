package com.gray.markdown.render.renderingrules

import com.gray.markdown.{MdHeader, MdLinkReference, MdParagraph}
import com.gray.string.AttributedString
import com.gray.string.domain.Format

import scala.io.AnsiColor

object MdHeaderRenderer extends MdParagraphRenderer {
  protected val BOLD_FORMAT = Format(other = Some(List(AnsiColor.BOLD)))
  protected val UNDERLINED_FORMAT = Format(other = Some(List(AnsiColor.UNDERLINED)))

  override def render(mdParagraph: MdParagraph,
                      width: Int,
                      linkRefs: List[MdLinkReference],
                      renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
                     ): Option[AttributedString] = mdParagraph match {
    case header: MdHeader => Some(renderHeader(header, width, linkRefs, renderer))
    case _ => None
  }

  protected def renderHeader(header: MdHeader,
                             width: Int,
                             linkRefs: List[MdLinkReference],
                             renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString) = {
    var headerString = renderer(header.mdString, width, linkRefs) << BOLD_FORMAT
    if (header.value <= 4) headerString = headerString.toUpperCase()
    if (header.value == 3) headerString = headerString << UNDERLINED_FORMAT
    if (header.value <= 2) {
      val underlineChar = if (header.value == 2) "─" else "═"
      val underline = concatenate(underlineChar, headerString.length)
      headerString = headerString + (AttributedString("\n" + underline) << BOLD_FORMAT)
    }
    headerString
  }
}
