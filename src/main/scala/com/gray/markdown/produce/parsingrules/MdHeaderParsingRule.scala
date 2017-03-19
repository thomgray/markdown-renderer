package com.gray.markdown.produce.parsingrules

import com.gray.markdown._

object MdHeaderParsingRule extends MdParsingRule{
  override def findParagraph(lines: List[String], marker: Int, offset: Int, parser: (List[String], Int) => MdDocument): Option[(MdParagraph, Int)] = findHeader(lines, marker, offset)


  def findHeader(lines: List[String], marker: Int, offset: Int) = headerRegex.findFirstMatchIn(lines(marker)) map { mtch =>
    (MdHeader(MdString(mtch.group(2), MdLocation(offset+marker, offset+marker+1)),
      mtch.group(1).length, MdLocation(offset+marker, offset+marker+1)), marker + 1)
  } match {
    case Some(res) => Some(res)
    case None if marker + 1 < lines.length && nonIndentedAnthingRegex.findFirstIn(lines(marker)).isDefined =>
      val nextLineTrimmed = lines(marker + 1).trim
      val indexOpt = if (nextLineTrimmed.matches("-{3,}")) Some(2)
      else if (nextLineTrimmed.matches("={3,}")) Some(1)
      else None
      indexOpt map (i => (MdHeader(MdString(lines(marker), MdLocation(offset+marker, offset+marker+1)),
        i, MdLocation(offset+marker, offset+marker+2)), marker + 2))
    case _ => None
  }
}
