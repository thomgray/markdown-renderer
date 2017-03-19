package com.gray.markdown.render.renderingrules

import com.gray.markdown.{MdLinkReference, MdParagraph, MdString}
import com.gray.string.AttributedString

object MdStringRenderer extends MdParagraphRenderer {

  override def render(mdParagraph: MdParagraph,
                      width: Int,
                      linkRefs: List[MdLinkReference],
                      renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
                     ) = mdParagraph match {
    case string: MdString =>
      regularSpacedString(string.string)    |
        (AttributedString(_))               |
        applyMdInlineFormatting             |
        (applyLinkHighlighting(_, linkRefs))|
        (_.wrapToWidth(width))              |
        Some.apply
    case _ => None
  }

}
