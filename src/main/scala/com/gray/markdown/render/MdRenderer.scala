package com.gray.markdown.render

import com.gray.markdown._
import com.gray.markdown.produce.MdCodeColouring
import com.gray.markdown.render.renderingrules._
import com.gray.markdown.render.rendertools.StringFormatting
import com.gray.string.AttributedString

trait MdRenderer extends StringFormatting with MdCodeColouring {

  val defaultRenderers = List(
    MdStringRenderer,
    MdCodeRenderer,
    MdQuoteRenderer,
    MdListRenderer,
    MdHeaderRenderer,
    MdTableRenderer
  )

  val renderers: List[MdParagraphRenderer] = defaultRenderers

  def render(document: MdDocument, width: Int): AttributedString = {
    val result = document.paragraphs.map(render(_, width, document.linkRefs))
    if (result.nonEmpty) result.reduce(_ + newParagraph + _) else AttributedString("")
  }

  def render(paragraph: MdParagraph, width: Int, linkRefs: List[MdLinkReference]): AttributedString = {
    renderers.mapOne(_.render(paragraph, width, linkRefs, render)) getOrElse AttributedString(paragraph.toString)
  }

}

object MdRenderer extends MdRenderer