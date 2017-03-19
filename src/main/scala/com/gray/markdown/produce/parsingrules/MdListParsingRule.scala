package com.gray.markdown.produce.parsingrules

import com.gray.markdown._
import com.gray.markdown.util.MapOne

import scala.util.matching.Regex

object MdListParsingRule extends MdParsingRule with MapOne {
  override def findParagraph(lines: List[String], marker: Int, offset: Int, parser: (List[String], Int) => MdDocument): Option[(MdParagraph, Int)] = findList(lines, marker, offset, parser)

  private val itemRegexes = List(bulletListItemRegex, checkListItemRegex, numberedListItemRegex)

  def findList(lines: List[String], marker: Int, offset: Int, parse: (List[String], Int) => MdDocument) = {
    itemRegexes mapOne (regex => regex.findFirstMatchIn(lines(marker)).map(mtch => (regex, mtch))) map { tuple =>
      val (regex, mtch) = tuple
      val (lastIndex, itemLines) = doMap(marker) { marker2 =>
        if (marker2 < lines.length) findListItem(lines, marker2, regex) map (tuple => (tuple._3, (tuple._1, tuple._2, MdLocation(offset+marker2, offset+tuple._3))))
        else None
      }
      val paragraphs = itemLines.map(l => {
        (parse(l._1, l._3.startLine).paragraphs, l._2, l._3)
      })
      val result = regex match {
        case `bulletListItemRegex` =>
          val items = paragraphs.map(l => MdBulletListItem(l._1, l._3))
          MdBulletList(items.toList, MdLocation(offset+marker, paragraphs.last._3.endLine)) -> lastIndex
        case `numberedListItemRegex` =>
          val items = paragraphs.map(l => MdNumberListItem(l._1, l._3))
          MdNumberList(items.toList, MdLocation(offset+marker, paragraphs.last._3.endLine)) -> lastIndex
        case `checkListItemRegex` =>
          val items = paragraphs.map(l => MdCheckListItem(l._1, l._2.contains("x"), l._3))
          MdChecktList(items.toList, MdLocation(offset+marker, paragraphs.last._3.endLine)) -> lastIndex
      }
      result
    }
  }

  /**
    * @param lines
    * @param marker
    * @param regex
    * @param prefixLength
    * @return
    * <dl>
    *   <dt>List[String]</dt>
    *   <dd>the lines for the list item</dd>
    *   <dt>String</dt>
    *   <dd>the list item prefix</dd>
    *   <dt>Int</dt>
    *   <dd>the end index of the list item </dd>
    * </dl>
    */
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
    Some(itemLines, prefix, lastLine)
  }
}
