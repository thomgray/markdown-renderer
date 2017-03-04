package com.gray.markdown.render.rendertools

import com.gray.string.AttributedString

trait StringFormatting {
  protected val newParagraph = AttributedString("\n\n")
  protected val newLine = AttributedString("\n")
  protected val indent = AttributedString("     ")

  private def wrapRegex(width: Int) = s"""^.{0,${width - 1}}(\\s|_|-|$$)""".r


  def regularSpacedString(string: String) =
    string.replaceAll("\\b {0,1}\n", " ").replaceAll(" +", " ").replaceAll(" \n", "\n")


  def wrapStringToWidth(string: String, width: Int, wrapPrefix: String = "", wrapSuffix: String = "") = {
    string.split("\n").map(wrapSingleLine(_, width, wrapPrefix, wrapSuffix)).mkString("\n")
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

  def drawBoxAround(attributedString: AttributedString) = {
    val lines = attributedString.split("\n")
    val longest = lines.foldLeft(0)((i, as) => if (as.length > i) as.length else i)
    val vBar = AttributedString("│")
    val block = blockify(lines, longest).map(l => vBar + l + vBar )
    val topBottom = concatenate("─", longest)
    val top = AttributedString(s"┌$topBottom┐")
    val bottom = AttributedString(s"└$topBottom┘")
    (top +: block :+ bottom).reduce(_+ AttributedString("\n") + _)
  }


  def blockify(lines: Seq[AttributedString], longest: Int): Seq[AttributedString] = lines.map(l => {
    if (l.length == longest) l
    else {
      val extra = longest - l.length
      val extraString  = AttributedString(concatenate(" ", extra))
      l + extraString
    }
  })

  def blockify(lines: Seq[AttributedString]): Seq[AttributedString] = {
    val longest = lines.foldLeft(0)((i, as) => if (as.length > i) as.length else i)
    blockify(lines, longest)
  }

  def prefixBlock(string: AttributedString, prefix: AttributedString) = {
    string.split("\n").map(prefix+_).reduce(_ + newLine + _)
  }

  def prefixBlock(string: AttributedString, firstLinePrefix: AttributedString, blockPrefix: AttributedString) = {
    val lines = string.split("\n")
    val first = firstLinePrefix + lines.head
    val remainder = lines.tail.map(blockPrefix + _)
    (first +: remainder).reduce(_ + newLine + _)
  }

  protected def concatenate(string: String, length: Int) = (0 until length).map(_ => string).mkString


}
