package com.gray.markdown.produce.parsingrules

import com.gray.markdown.{MdCode, MdDocument, MdLocation, MdParagraph}

object MdCodeParsingRule extends MdParsingRule {
  override def findParagraph(lines: List[String], marker: Int, offset: Int, parser: (List[String], Int) => MdDocument): Option[(MdParagraph, Int)] = findCodeBlock(lines, marker, offset) orElse findIndentedCodeBlock(lines, marker, offset)

  def findCodeBlock(lines: List[String], marker: Int, offset: Int) = codeBeginRegex.findFirstMatchIn(lines(marker)) flatMap { mtch =>
    val language = Option(mtch.group(2))
    val endIndex = lines.indexWhere(codeEndRegex.findFirstIn(_).isDefined, marker + 1)
    if (endIndex > 0) {
      val codeLines = lines.slice(marker + 1, endIndex)
      val code = MdCode(codeLines.mkString("\n"), language, MdLocation(offset+marker, offset+endIndex+1))
      Some(code, endIndex + 1)
    } else None
  }

  def findIndentedCodeBlock(lines: List[String], marker: Int, offset: Int) = indentedLiteralRegex.findFirstIn(lines(marker)) flatMap { mtch =>
    val endIndex = lines.indexWhere(line => indentedLiteralRegex.findFirstIn(line).isEmpty && emptyLineRegex.findFirstIn(line).isEmpty, marker + 1) match {
      case -1 => lines.length
      case other => other
    }
    val string = lines.slice(marker, endIndex).map(_.stripPrefix("    ")).mkString("\n")
    Some(MdCode(string, None, MdLocation(offset+marker, offset+endIndex)), endIndex)
  }

}
