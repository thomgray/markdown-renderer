package com.gray.markdown.produce.parsingrules

import com.gray.markdown.{MdDocument, MdLocation, MdParagraph, MdQuote}

object MdQuoteParsingRule extends MdParsingRule {

  override def findParagraph(lines: List[String], marker: Int, offset: Int, parser: (List[String], Int) => MdDocument): Option[(MdParagraph, Int)] = findQuoteBlock(lines, marker, offset)

  def findQuoteBlock(lines: List[String], marker: Int, offset: Int) = blockQuoteRegex.findFirstIn(lines(marker)) flatMap { _ =>
    val end = lines.indexWhere(string => continuingBlockQuoteRegex.findFirstIn(string).isEmpty, marker+1) match {
      case -1 => lines.length
      case other => other
    }
    val string = lines.slice(marker, end).map(_.replaceAll("^ *> *", "")).mkString(" ")
    Some(MdQuote(string, MdLocation(offset+marker, offset+end)), end)
  }


}
