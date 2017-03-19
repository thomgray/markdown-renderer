package com.gray.markdown.render

import com.gray.markdown._
import com.gray.markdown.produce.MdCodeColouring
import com.gray.markdown.render.renderingrules._
import com.gray.markdown.render.rendertools.StringFormatting
import com.gray.string.AttributedString
import com.gray.string.domain.Format

import scala.io.AnsiColor

trait MdRenderer extends StringFormatting with MdCodeColouring {

  val defaultRenderers = List(
    MdStringRenderer,
    MdCodeRenderer,
    MdQuoteRenderer,
    MdListRenderer,
    MdHeaderRenderer
  )

  val renderers: List[MdParagraphRenderer] = defaultRenderers

  def render(document: MdDocument, width: Int): AttributedString = {
    val result = document.paragraphs.map(render(_, width, document.linkRefs))
    if (result.nonEmpty) result.reduce(_ + newParagraph + _) else AttributedString("")
  }

  def render(paragraph: MdParagraph, width: Int, linkRefs: List[MdLinkReference]): AttributedString = {
    renderers.mapOne(_.render(paragraph, width, linkRefs, render)) getOrElse AttributedString(paragraph.toString)
  }

//  protected def renderString(mdString: MdString, width: Int, linkRefs: List[MdLinkReference] = Nil): AttributedString = {
//    regularSpacedString(mdString.string)  |
//      (AttributedString(_))               |
//      applyMdInlineFormatting             |
//      (applyLinkHighlighting(_, linkRefs))|
//      (_.wrapToWidth(width))
//  }
//
//  protected def renderCode(mdCode: MdCode, width: Int) = {
//    val codeString = colourCode(mdCode.string, mdCode.language).wrapToWidth(width-2)
//    colourBackground(codeString, width)
//  }
//
//  protected def renderHeader(header: MdHeader, width: Int) = {
//    var headerString = renderString(header.mdString, width) << BOLD_FORMAT
//    if (header.value <= 4) headerString = headerString.toUpperCase()
//    if (header.value == 3) headerString = headerString << UNDERLINED_FORMAT
//    if (header.value <= 2) {
//      val underlineChar = if (header.value == 2) "─" else "═"
//      val underline = concatenate(underlineChar, headerString.length)
//      headerString = headerString + (AttributedString("\n" + underline) << BOLD_FORMAT)
//    }
//    headerString
//  }
//
//  protected def renderList(mdList: MdList[MdListItem], width: Int, linkRefs: List[MdLinkReference]): AttributedString = {
//    renderListAtTier(mdList, width, linkRefs)
//  }
//
//  protected def renderListAtTier(mdList: MdList[MdListItem], width: Int, linkRefs: List[MdLinkReference], tier: Int = 0): AttributedString = {
//    val trueWidth = width - indent.length
//
//    var x = 1
//    (mdList.items map { item =>
//      val paragraphs = item.paragraphs.map({
//        case list: MdList[MdListItem] => renderListAtTier(list, trueWidth, linkRefs, tier+1)
//        case other => render(other, trueWidth, linkRefs)
//      })
//
//      val prefix = item match {
//        case _: MdBulletListItem => prefixForBullet(tier)
//        case cli: MdCheckListItem => prefixForCheck(cli.checked)
//        case _: MdNumberListItem => prefixForNumber(x)
//      }
//
//      x += 1
//
//      val headBlock = prefixBlock(paragraphs.head, prefix, indent)
//      val tailBlock = paragraphs.tail.map(prefixBlock(_, indent))
//      (headBlock +: tailBlock).reduce(_ + newParagraph + _)
//    }).reduce(_ + newLine + _)
//
//  }
//
//  private def prefixForBullet(tier: Int) = {
//    (tier match {
//      case 0 => AttributedString("   • ")
//      case 1 => AttributedString("   ◦ ")
//      case _ => AttributedString("   ⁃ ")
//    }) << Format(foreground = Some(AnsiColor.BLUE))
//  }
//
//  private def prefixForNumber(number: Int) = {
//    val numberString = number.toString
//    val beforeString = if (numberString.length > 3) {
//      numberString
//    } else {
//      "".padTo(3-numberString.length, ' ') + numberString
//    }
//    (AttributedString(s"$beforeString") << Format(foreground = Some(AnsiColor.BLUE))) + AttributedString(". ")
//  }
//
//  private def prefixForCheck(checked: Boolean) = {
//    val box = if (checked) "☒" else "☐"
//    AttributedString(s"   $box ")
//  }
//
//  val quotePrefix = (AttributedString(" ") << Format(background = Some(AnsiColor.WHITE_B))) + AttributedString(" ")
//
//  protected def renderQuote(mdQuote: MdQuote, length: Int) = {
//    val normalisedString = wrapStringToWidth(regularSpacedString(mdQuote.string), length-2)
//    normalisedString.split("\n").map{ s =>
//      quotePrefix + AttributedString(s)
//    }.reduce(_ + newLine + _)
//  }

}

object MdRenderer extends MdRenderer