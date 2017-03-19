package com.gray.markdown.produce.parsingrules

import com.gray.markdown._
import com.gray.markdown.util.MapOne

object MdTableParsingRule extends MdParsingRule with MapOne {

  val ALIGN_LEFT = 0
  val ALIGN_CENTRE = 1
  val ALIGN_RIGHT = 2

  override def findParagraph(lines: List[String], marker: Int, offset: Int, parser: (List[String], Int) => MdDocument): Option[(MdParagraph, Int)] = findTable(lines, marker, offset)

  protected val nextCellRegex = "^\\|?.+?(?:\\||$)".r
  protected val tableRowRegex = "^ {0,4}(.*\\|.*)$".r

  def findTable(lines: List[String], marker: Int, offset: Int): Option[(MdTable, Int)] = if (marker + 2 < lines.length) {
    for {
      headerString <- tableHeaderRegex.findFirstIn(lines(marker))
      headers = getTableRowRecursive(headerString, marker)
      colNumber = headers.length
      colScheme <- getTableColumnScheme(lines(marker + 1))
      (lastLine, tableData) = getTableData(lines, marker + 2)
    } yield {
      val location = MdLocation(marker, lastLine)
      val table = MdTable(headers, tableData.toList, colScheme, location)
      (table, lastLine)
    }
  } else None

  protected def getTableRow(string: String, line: Int) = tableRowRegex.findFirstIn(string) map { _ =>
    getTableRowRecursive(string, line, 0, Nil)
  }

  private def getTableRowRecursive(string: String, line: Int, colOffset: Int = 0, sofar: List[MdString] = Nil): List[MdString] = {
    getNextCellItem(string) match {
      case None => sofar
      case Some((mtch, remainder)) =>
        val cleanMatch = mtch.stripPrefix("|").stripSuffix("|").trim
        val location = MdLocation(line, line + 1, colOffset, colOffset + mtch.length)
        //TODO not really the string location
        val newOffset = colOffset + mtch.length
        val mdString = MdString(cleanMatch, location)
        getTableRowRecursive(remainder, line, newOffset, sofar :+ mdString)
    }
  }

  protected def getTableColumnScheme(string: String) = "^ {0,4}[|:\\- ]+$".r.findFirstIn(string) map { _ =>
    def recursive(remaining: String, soFar: List[Int]): List[Int] = {
      getNextCellItem(remaining) match {
        case None => soFar
        case Some((mtch, rest)) =>
          val mtch1 = mtch.stripPrefix("|").stripSuffix("|").trim
          val alignment =
            if (mtch1.startsWith(":") && mtch.endsWith(":")) ALIGN_CENTRE
            else if (mtch.endsWith(":")) ALIGN_RIGHT
            else ALIGN_LEFT
          recursive(rest, soFar :+ alignment)
      }
    }
    recursive(string, Nil)
  }

  protected def getTableData(lines: List[String], marker: Int) = doMap(marker) { m =>
    if (m < lines.length) getTableRow(lines(m), m) map (l => (m + 1, MdTableRow(l)))
    else None
  }


  protected def getNextCellItem(string: String) = nextCellRegex.findFirstIn(string) map { mtch =>
    (mtch, string.stripPrefix(mtch))
  }

}
