package com.gray.markdown.produce.parsingrules

import com.gray.markdown._

object MdTableParsingRule extends MdParsingRule {
  override def findParagraph(lines: List[String], marker: Int, offset: Int, parser: (List[String], Int) => MdDocument): Option[(MdParagraph, Int)] = findTable(lines, marker, offset)

  def findTable(lines: List[String], marker: Int, offset: Int): Option[(MdTable, Int)] = tableHeaderRegex.findFirstMatchIn(lines(marker)) flatMap { mtch =>
    val headers = getTableColumn(lines(marker), marker)
    val colInfo = getTableColumn(lines(marker+1), marker+1)
    val colNumber = headers.length
    val tableRow1 = getTableColumn(lines(marker+2), marker+1)
    val row = MdTableRow(tableRow1)
    val table = MdTable(
      headers, List(row), colInfo.map(_ => 0), MdLocation(marker, marker+3)
    )
    Some(table, marker+3)
  }

  protected def getTableColumn(string: String, line: Int, colOffset: Int = 0, sofar: List[MdString] = Nil): List[MdString] = {
    tableColItemRegex.findFirstMatchIn(string) match {
      case None => sofar
      case Some(mtch) =>
        val newString = string.stripPrefix(mtch.group(0))
        val location = MdLocation(line, line+1, colOffset+mtch.start, colOffset+mtch.end)
        //TODO not really the string location
        val newOffset = colOffset+mtch.end
        val mdString = MdString(mtch.group(1), location)
        getTableColumn(newString, line, newOffset, sofar :+ mdString)
    }
  }

}
