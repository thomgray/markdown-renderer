package com.gray.markdown.produce.parsingrules

import com.gray.markdown.produce.MdRegexes
import com.gray.markdown.{MdDocument, MdParagraph}

trait MdParsingRule extends MdRegexes {
  def findParagraph(lines: List[String],
                    marker: Int,
                    offset: Int,
                    parser: (List[String], Int) => MdDocument
                   ): Option[(MdParagraph, Int)]

}
