package com.gray.markdown.produce

import com.gray.markdown._
import com.gray.markdown.produce.parsingrules._
import com.gray.markdown.util.MapOne

import scala.collection.mutable

trait MdParser extends MdRegexes with MapOne {

  protected def parse(lines: List[String], offset: Int): MdDocument = {
    var mark = 0
    val paragraphs = mutable.ListBuffer[MdParagraph]()
    val stringBuffer = mutable.ListBuffer[String]()

    def flushStringBuffer = if (stringBuffer.nonEmpty) {
      val location = MdLocation(mark - stringBuffer.length + offset, mark + offset)
      val newStringParagraph = MdString(stringBuffer.mkString("\n"), location)
      paragraphs.append(newStringParagraph)
      stringBuffer.clear()
    }

    while (mark < lines.length) {
      checks mapOne (_.findParagraph(lines, mark, offset, parse)) match {
        case Some((MdBreak, newLine)) =>
          flushStringBuffer
          mark = newLine
        case Some((paragraph, newLine)) =>
          flushStringBuffer
          paragraphs.append(paragraph)
          mark = newLine
        case None =>
          stringBuffer += lines(mark)
          mark += 1
      }
    }

    flushStringBuffer

    MdDocument(paragraphs.toList)
  }

  def parse(string: String): MdDocument = parse(string.split("\n").toList, 0)

  val defaultChecks: List[MdParsingRule] = List(
    MdBreakParsingRule,
    MdHeaderParsingRule,
    MdQuoteParsingRule,
    MdCodeParsingRule,
    MdListParsingRule,
    MdTableParsingRule
  )

  val checks = defaultChecks

}

object MdParser extends MdParser