package com.gray.markdown.render.renderingrules

import com.gray.markdown.{MdLinkReference, MdParagraph, MdQuote}
import com.gray.string.AttributedString
import com.gray.string.domain.Format

import scala.io.AnsiColor

object MdQuoteRenderer extends MdParagraphRenderer {

  val quotePrefix = (AttributedString(" ") << Format(background = Some(AnsiColor.WHITE_B))) + AttributedString(" ")

  override def render(mdParagraph: MdParagraph,
                      width: Int,
                      linkRefs: List[MdLinkReference],
                      renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
                     ): Option[AttributedString] = mdParagraph match {
    case quote: MdQuote =>
      val normalisedString = wrapStringToWidth(regularSpacedString(quote.string), width-2)
      normalisedString.split("\n").map{ s =>
        quotePrefix + AttributedString(s)
      }.reduce(_ + newLine + _) | Some.apply
    case _ => None
  }


}
