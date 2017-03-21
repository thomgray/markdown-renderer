package com.gray.markdown.render.renderingrules

import com.gray.markdown.render.codecolourers._
import com.gray.markdown.{MdCode, MdLinkReference, MdParagraph}
import com.gray.string.AttributedString

object MdCodeRenderer extends MdParagraphRenderer {

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

  def colourCode(string: String, languageOpt: Option[String]) = {
    languageOpt match {
      case Some(language) if codeColourers.contains(language) =>
        codeColourers(language).colourCode(AttributedString(string))
      case _ => AttributedString(string)
    }
  }

  val codeColourers: Map[String, MdCodeColouring] = Map(
    "json" -> MdJsonCodeColourer,
    "scala" -> MdScalaCodeColourer,
    "php" -> MdPhpCodeColourer,
    "sh" -> MdBashCodeColourer,
    "ss" -> MdBashCodeColourer
  )

}
