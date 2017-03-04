package com.gray.markdown.render

import com.gray.markdown._
import com.gray.markdown.produce.MdCodeColouring
import com.gray.markdown.render.rendertools.StringFormatting
import com.gray.string.AttributedString
import com.gray.string.domain.Format

import scala.io.AnsiColor

trait MdRenderer extends StringFormatting with MdCodeColouring {
  protected val BOLD_FORMAT = Format(other = Some(List(AnsiColor.BOLD)))
  protected val UNDERLINED_FORMAT = Format(other = Some(List(AnsiColor.UNDERLINED)))

  def render(document: MdDocument, width: Int): AttributedString = {
    val result = document.paragraphs.map(render(_, width))
    result.reduce(_ + newParagraph + _)
  }

  def render(paragraph: MdParagraph, width: Int): AttributedString = paragraph match {
    case string: MdString => renderString(string, width)
    case code: MdCode => renderCode(code, width)
    case list: MdList[MdListItem] => renderList(list, width)
  }

  protected def renderString(mdString: MdString, width: Int): AttributedString = {
    val stringWithRegularSpacing = mdString.string.replaceAll("\\b {0,1}\n", " ").replaceAll(" +", " ").replaceAll(" \n", "\n")
    val wrapped = wrapStringToWidth(stringWithRegularSpacing, width)
    AttributedString(wrapped)
  }

  protected def renderCode(mdCode: MdCode, width: Int) = {
    val codeString = colourCode(mdCode.string, mdCode.language)
    drawBoxAround(codeString)
  }

  protected def renderHeader(header: MdHeader, width: Int) = {
    var headerString = renderString(header.mdString, width) << BOLD_FORMAT
    if (header.value <= 4) headerString = headerString.toUpperCase()
    if (header.value == 3) headerString = headerString << UNDERLINED_FORMAT
    if (header.value <= 2) {
      val underlineChar = if (header.value == 2) "─" else "═"
      val underline = concatenate(underlineChar, headerString.length)
      headerString = headerString + (AttributedString("\n" + underline) << BOLD_FORMAT)
    }
    headerString
  }

  protected def renderList(mdList: MdList[MdListItem], width: Int): AttributedString = {
    renderListAtTier(mdList, width)
  }

  protected def renderListAtTier(mdList: MdList[MdListItem], width: Int, tier: Int = 0): AttributedString = {
    val trueWidth = width - indent.length

    var x = 1
    (mdList.items map { item =>
      val paragraphs = item.paragraphs.map({
        case list: MdList[MdListItem] => renderListAtTier(list, trueWidth, tier+1)
        case other => render(other, trueWidth)
      })

      val prefix = item match {
        case _: MdBulletListItem => prefixForBullet(tier)
        case cli: MdCheckListItem => prefixForCheck(cli.checked)
        case _: MdNumberListItem => prefixForNumber(x)
      }

      x += 1

      val headBlock = prefixBlock(paragraphs.head, prefix, indent)
      val tailBlock = paragraphs.tail.map(prefixBlock(_, indent))
      (headBlock +: tailBlock).reduce(_ + newParagraph + _)
    }).reduce(_ + newLine + _)

  }

  private def prefixForBullet(tier: Int) = {
    (tier match {
      case 0 => AttributedString("   • ")
      case 1 => AttributedString("   ◦ ")
      case _ => AttributedString("   ⁃ ")
    }) << Format(foreground = Some(AnsiColor.BLUE))
  }

  private def prefixForNumber(number: Int) = {
    val numberString = number.toString
    val beforeString = if (numberString.length > 3) {
      numberString
    }else {
      "".padTo(3-numberString.length, ' ') + numberString
    }
    (AttributedString(s"$beforeString") << Format(foreground = Some(AnsiColor.BLUE))) + AttributedString(". ")
  }

  private def prefixForCheck(checked: Boolean) = {
    val box = if (checked) "☒" else "☐"
    AttributedString(s"   $box ")
  }

}

object MdRenderer extends MdRenderer