package com.gray.markdown.render.renderingrules

import com.gray.markdown.render.rendertools.StringFormatting
import com.gray.markdown.{MdLinkReference, MdParagraph}
import com.gray.string.AttributedString

trait MdParagraphRenderer extends StringFormatting {

  def render(mdParagraph: MdParagraph,
             width: Int,
             linkRefs: List[MdLinkReference],
             renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
            ): Option[AttributedString]

}
