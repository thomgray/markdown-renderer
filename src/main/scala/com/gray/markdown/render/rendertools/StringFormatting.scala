package com.gray.markdown.render.rendertools

import com.gray.markdown.MdLinkReference
import com.gray.markdown.produce.MdRegexes
import com.gray.markdown.util.MapOne
import com.gray.string.AttributedString
import com.gray.string.domain.{Attribute, Format}

import scala.io.AnsiColor

trait StringFormatting extends MdRegexes with MapOne {
  protected val codeBackgroundFormat = Format(background = Some(AnsiColor.BLACK_B))
  protected val newParagraph = AttributedString("\n\n")
  protected val newLine = AttributedString("\n")
  protected val indent = AttributedString("     ")

  protected val urlFormat = Format(foreground = Some(AnsiColor.BLUE))

  private val emptyCodeSpace = AttributedString(" ") << codeBackgroundFormat


  private def wrapRegex(width: Int) = s"""^.{0,${width - 1}}(\\s|_|-|$$)""".r


  def regularSpacedString(string: String) =
    string.replaceAll("\\b {0,1}\n", " ").replaceAll(" +", " ").replaceAll(" \n", "\n")


  def wrapStringToWidth(string: String, width: Int, wrapPrefix: String = "", wrapSuffix: String = "") = {
    if (width > 0) string.split("\n").map(wrapSingleLine(_, width, wrapPrefix, wrapSuffix)).mkString("\n")
    else string
  }

  protected def wrapSingleLine(singleLine: String, width: Int, wrapPrefix: String, wrapSuffix: String): String = {
    val whiteSpaceInFrontOfFirstLine = "^\\s*".r.findFirstIn(singleLine).getOrElse("")
    var outLine = wrapRegex(width).findFirstIn(singleLine).getOrElse(singleLine.substring(0, Math.min(width, singleLine.length))).stripSuffix(" ")
    var remainingInLine = singleLine.stripPrefix(outLine).trim
    if (remainingInLine.length > 0) outLine += wrapSuffix
    val wrapWidth = width - whiteSpaceInFrontOfFirstLine.length - wrapPrefix.length
    val wrapReg = wrapRegex(wrapWidth)

    while (remainingInLine.length > 0) {
      val nextLine = wrapReg.findFirstIn(remainingInLine).getOrElse(remainingInLine.substring(0, Math.min(wrapWidth, remainingInLine.length))).trim
      outLine += "\n" + whiteSpaceInFrontOfFirstLine + wrapPrefix + nextLine + wrapSuffix
      remainingInLine = remainingInLine.stripPrefix(nextLine).trim
    }
    outLine
  }

  def drawBoxAround(attributedString: AttributedString, width: Int) = {
    val lines = attributedString.split("\n")
    val vBar = AttributedString("│")
    val block = blockify(lines, width - 2).map(l => vBar + l + vBar)
    val topBottom = concatenate("─", width - 2)
    val top = AttributedString(s"┌$topBottom┐")
    val bottom = AttributedString(s"└$topBottom┘")
    (top +: block :+ bottom).reduce(_ + newLine + _)
  }

  def applyMdInlineFormatting(attributedString: AttributedString) = {
    applyBoldFormat(attributedString) |
      applyUnderlinedFormat |
      applyInlineCodeFormat
  }

  private def applyBoldFormat(attributedString: AttributedString): AttributedString = {
    boldRegex.findFirstMatchIn(attributedString.string) match {
      case None => attributedString
      case Some(mtch) =>
        val before = attributedString.subString(end = mtch.start)
        val after = attributedString.subString(mtch.end)
        val bit = attributedString.subString(mtch.start + 2, mtch.end - 2) << Format(other = Some(List(AnsiColor.BOLD)))
        applyBoldFormat(before + bit + after)
    }
  }

  private def applyUnderlinedFormat(attributedString: AttributedString): AttributedString = {
    italicRegex.findFirstMatchIn(attributedString.string) match {
      case None => attributedString
      case Some(mtch) =>
        val before = attributedString.subString(end = mtch.start)
        val after = attributedString.subString(mtch.end)
        val bit = attributedString.subString(mtch.start + 1, mtch.end - 1) << Format(other = Some(List(AnsiColor.UNDERLINED)))
        applyUnderlinedFormat(before + bit + after)
    }
  }

  private def applyInlineCodeFormat(attributedString: AttributedString): AttributedString = {
    inlineCodeRegex.findFirstMatchIn(attributedString.string) match {
      case None => attributedString
      case Some(mtch) =>
        val before = attributedString.subString(end = mtch.start)
        val after = attributedString.subString(mtch.end)
        val bit = attributedString.subString(mtch.start + 1, mtch.end - 1) << codeBackgroundFormat
        applyInlineCodeFormat(before + bit + after)
    }
  }

  def applyLinkHighlighting(attributedString: AttributedString, linkRefs: List[MdLinkReference] = Nil) = {
    applyLabeledLinkHighlighting(attributedString) |
      applyPlainLinkHighlighting |
      (s => applyRefLinkHighlighting(s, linkRefs))
  }

  protected def applyRefLinkHighlighting(attributedString: AttributedString, refs: List[MdLinkReference]) =
    inSquareBracesRegex.findAllMatchIn(attributedString.string).
      toList.foldRight(attributedString) { (mtch, string) =>
      val label = mtch.group(1) match {
        case null => mtch.group(2)
        case other => other
      }
      val ref = mtch.group(2)
      refs.find(_.label == ref) match {
        case Some(reference) =>
          val res = string.subString(end = mtch.start) + (AttributedString(label) << urlFormat) + string.subString(mtch.end)
          res
        case None => string
      }
    }

  protected def applyLabeledLinkHighlighting(attributedString: AttributedString) =
    mdLinkRegex.findAllMatchIn(attributedString.string)
      .toList.foldRight(attributedString) { (mtch, string) =>
      string.subString(end = mtch.start) +
        (AttributedString(mtch.group(1)) << urlFormat) +
        string.subString(mtch.end)
    }

  protected def applyPlainLinkHighlighting(attributedString: AttributedString) =
    urlRegex.findAllMatchIn(attributedString.string).
      toList.foldRight(attributedString) { (mtch, string) =>
      string < Attribute(urlFormat, mtch.start, mtch.end)
    }

  def colourBackground(attributedString: AttributedString, width: Int) = {
    val extraLine = AttributedString(concatenate(" ", width)) << codeBackgroundFormat
    val lines = attributedString.split("\n")
    val block = blockify(lines, width - 2).map(l => emptyCodeSpace + (l << codeBackgroundFormat) + emptyCodeSpace)
    (extraLine +: block :+ extraLine).reduce(_ + newLine + _)
  }

  def drawBoxAround(attributedString: AttributedString) = {
    val lines = attributedString.split("\n")
    val longest = lines.foldLeft(0)((i, as) => if (as.length > i) as.length else i)
    val vBar = AttributedString("│")
    val block = blockify(lines, longest).map(l => vBar + l + vBar)
    val topBottom = concatenate("─", longest)
    val top = AttributedString(s"┌$topBottom┐")
    val bottom = AttributedString(s"└$topBottom┘")
    (top +: block :+ bottom).reduce(_ + newLine + _)
  }


  def blockify(lines: Seq[AttributedString], longest: Int): Seq[AttributedString] = lines.map(l => {
    if (l.length == longest) l
    else {
      val extra = longest - l.length
      val extraString = AttributedString(concatenate(" ", extra))
      l + extraString
    }
  })

  def blockify(lines: Seq[AttributedString]): Seq[AttributedString] = {
    val longest = lines.foldLeft(0)((i, as) => if (as.length > i) as.length else i)
    blockify(lines, longest)
  }

  def prefixBlock(string: AttributedString, prefix: AttributedString) = {
    string.split("\n").map(prefix + _).reduce(_ + newLine + _)
  }

  def prefixBlock(string: AttributedString, firstLinePrefix: AttributedString, blockPrefix: AttributedString) = {
    val lines = string.split("\n")
    val first = firstLinePrefix + lines.head
    val remainder = lines.tail.map(blockPrefix + _)
    (first +: remainder).reduce(_ + newLine + _)
  }

  protected def concatenate(string: String, length: Int) = (0 until length).map(_ => string).mkString


}
