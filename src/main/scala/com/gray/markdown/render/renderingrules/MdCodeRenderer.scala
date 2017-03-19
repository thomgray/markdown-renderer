package com.gray.markdown.render.renderingrules

import com.gray.markdown.produce.MdCodeColouring
import com.gray.markdown.{MdCode, MdLinkReference, MdParagraph}
import com.gray.string.AttributedString

object MdCodeRenderer extends MdParagraphRenderer with MdCodeColouring {

  override def render(mdParagraph: MdParagraph,
                      width: Int,
                      linkRefs: List[MdLinkReference],
                      renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
                     ): Option[AttributedString] = mdParagraph match {
    case MdCode(string, language, _) =>
      val codeString = colourCode(string, language).wrapToWidth(width-2)
      colourBackground(codeString, width) | Some.apply
    case _ => None
  }

}
