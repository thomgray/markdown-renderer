package com.gray.markdown.produce.parsingrules

import com.gray.markdown.{MdBreak, MdDocument, MdParagraph}

object MdBreakParsingRule extends MdParsingRule {
  override def findParagraph(lines: List[String], marker: Int, offset: Int, parser: (List[String], Int) => MdDocument): Option[(MdParagraph, Int)] = emptyLineRegex.findFirstIn(lines(marker)) map (_ => (MdBreak, marker + 1))
}
