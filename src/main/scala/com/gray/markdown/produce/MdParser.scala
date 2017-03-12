package com.gray.markdown.produce

import com.gray.markdown.util.MapOne
import com.gray.markdown._

import scala.collection.mutable
import scala.util.matching.Regex

trait MdParser extends MdRegexes with MdFactory with MapOne {

  def parse(string: String): MdDocument = {

    val lines = string.split("(\n|\r)").toList
    var mark = 0
    val paragraphs = mutable.ListBuffer[MdParagraph]()
    val stringBuffer = mutable.ListBuffer[String]()

    def flushStringBuffer = if (stringBuffer.nonEmpty) {
      val location = MdLocation(mark-stringBuffer.length, mark)
      val newStringParagraph = MdString(stringBuffer.mkString("\n"), location)
      paragraphs.append(newStringParagraph)
      stringBuffer.clear()
    }

    while (mark < lines.length) {
      checks mapOne (_ (lines, mark)) match {
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

  val checks: List[(List[String], Int) => Option[(MdParagraph, Int)]] = List(
    findBreak,
    findCodeBlock,
    findIndentedCodeBlock,
    findQuoteBlock,
    findList,
    findHeader
  )

  def findBreak(lines: List[String], marker: Int): Option[(MdParagraph, Int)] =
    emptyLineRegex.findFirstIn(lines(marker)) map (_ => (MdBreak, marker + 1))


  def findCodeBlock(lines: List[String], marker: Int) = codeBeginRegex.findFirstMatchIn(lines(marker)) flatMap { mtch =>
    val language = Option(mtch.group(2))
    val endIndex = lines.indexWhere(codeEndRegex.findFirstIn(_).isDefined, marker + 1)
    if (endIndex > 0) {
      val codeLines = lines.slice(marker + 1, endIndex)
      val code = MdCode(codeLines.mkString("\n"), language, MdLocation(marker, endIndex+1))
      Some(code, endIndex + 1)
    } else None
  }

  def findQuoteBlock(lines: List[String], marker: Int) = blockQuoteRegex.findFirstIn(lines(marker)) flatMap { _ =>
    val end = lines.indexWhere(string => continuingBlockQuoteRegex.findFirstIn(string).isEmpty, marker+1) match {
      case -1 => lines.length
      case other => other
    }
    val string = lines.slice(marker, end).map(_.replaceAll("^ *> *", "")).mkString(" ")
    Some(MdQuote(string, MdLocation(marker, end)), end)
  }

  def findIndentedCodeBlock(lines: List[String], marker: Int) = indentedLiteralRegex.findFirstIn(lines(marker)) flatMap { mtch =>
    val endIndex = lines.indexWhere(line => indentedLiteralRegex.findFirstIn(line).isEmpty && emptyLineRegex.findFirstIn(line).isEmpty, marker + 1) match {
      case -1 => lines.length
      case other => other
    }
    val string = lines.slice(marker, endIndex).map(_.stripPrefix("    ")).mkString("\n")
    Some(MdCode(string, None, MdLocation(marker, endIndex)), endIndex)
  }

  private val itemRegexes = List(bulletListItemRegex, checkListItemRegex, numberedListItemRegex)

  def findList(lines: List[String], marker: Int) = {
    itemRegexes mapOne (regex => regex.findFirstMatchIn(lines(marker)).map(mtch => (regex, mtch))) map { tuple =>
      val (regex, mtch) = tuple
      val (lastIndex, itemLines) = doMap(marker) { marker2 =>
        if (marker2 < lines.length) findListItem(lines, marker2, regex) map (tuple => (tuple._2, (tuple._1._1, tuple._1._2, MdLocation(marker2, tuple._2))))
        else None
      }
      val paragraphs = itemLines.map(l => (parse(l._1.mkString("\n")).paragraphs, l._2, l._3))
      val result = regex match {
        case `bulletListItemRegex` =>
          val items = paragraphs.map(l => MdBulletListItem(l._1, l._3))
          MdBulletList(items.toList, MdLocation(marker, paragraphs.last._3.endLine)) -> lastIndex
        case `numberedListItemRegex` =>
          val items = paragraphs.map(l => MdNumberListItem(l._1, l._3))
          MdNumberList(items.toList, MdLocation(marker, paragraphs.last._3.endLine)) -> lastIndex
        case `checkListItemRegex` =>
          val items = paragraphs.map(l => MdCheckListItem(l._1, l._2.contains("x"), l._3))
          MdChecktList(items.toList, MdLocation(marker, paragraphs.last._3.endLine)) -> lastIndex
      }
      result
    }
  }

  def findListItem(lines: List[String], marker: Int, regex: Regex, prefixLength: Int = 0) = regex.findFirstMatchIn(lines(marker)) flatMap { mtch =>
    val prefix = mtch.group(1)
    val prefixLength = regex match {
      case `checkListItemRegex` => checkListItemPrefix.findFirstIn(prefix).getOrElse("").length
      case _ => prefix.length
    }
    val blankPrefix = (0 until prefixLength).map(s => " ").mkString
    val body = mtch.group(2)
    var lastLineBlank = false
    val lastLine = lines.indexWhere(line => {
      val thisLineBlank = emptyLineRegex.findFirstIn(line).isDefined
      val breakDueToDoubleBlank = thisLineBlank && lastLineBlank
      lastLineBlank = thisLineBlank
      (!line.startsWith(blankPrefix) && !thisLineBlank) || breakDueToDoubleBlank
    }, marker + 1) match {
      case -1 => lines.length
      case other => other
    }
    val itemLines = body +: lines.slice(marker + 1, lastLine)
      .map(_.stripPrefix(blankPrefix))
    Some((itemLines, prefix), lastLine)
  }

  def findHeader(lines: List[String], marker: Int) = headerRegex.findFirstMatchIn(lines(marker)) map { mtch =>
    (MdHeader(MdString(mtch.group(2), MdLocation(marker, marker+1)),
      mtch.group(1).length, MdLocation(marker, marker+1)), marker + 1)
  } match {
    case Some(res) => Some(res)
    case None if marker + 1 < lines.length && nonIndentedAnthingRegex.findFirstIn(lines(marker)).isDefined =>
      val nextLineTrimmed = lines(marker + 1).trim
      val indexOpt = if (nextLineTrimmed.matches("-{3,}")) Some(2)
      else if (nextLineTrimmed.matches("={3,}")) Some(1)
      else None
      indexOpt map (i => (MdHeader(MdString(lines(marker), MdLocation(marker, marker+1)), i, MdLocation(marker, marker+1)), marker + 2))
    case _ => None
  }


}

object MdParser extends MdParser