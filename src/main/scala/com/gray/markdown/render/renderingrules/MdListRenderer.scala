package com.gray.markdown.render.renderingrules

import com.gray.markdown._
import com.gray.string.AttributedString
import com.gray.string.domain.Format

import scala.io.AnsiColor

object MdListRenderer extends MdParagraphRenderer {
  override def render(mdParagraph: MdParagraph,
                      width: Int,
                      linkRefs: List[MdLinkReference],
                      renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
                     ): Option[AttributedString] = mdParagraph match {
    case list: MdList[MdListItem] => Some(renderListAtTier(list, width, linkRefs, renderer))
    case _ => None
  }

  protected def renderListAtTier(mdList: MdList[MdListItem],
                                 width: Int,
                                 linkRefs: List[MdLinkReference],
                                 renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString,
                                 tier: Int = 0): AttributedString = {
    val trueWidth = width - indent.length

    var x = 1
    (mdList.items map { item =>
      val paragraphs = item.paragraphs.map({
        case list: MdList[MdListItem] => renderListAtTier(list, trueWidth, linkRefs, renderer, tier+1)
        case other => renderer(other, trueWidth, linkRefs)
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
    } else {
      "".padTo(3-numberString.length, ' ') + numberString
    }
    (AttributedString(s"$beforeString") << Format(foreground = Some(AnsiColor.BLUE))) + AttributedString(". ")
  }

  private def prefixForCheck(checked: Boolean) = {
    val box = if (checked) "☒" else "☐"
    AttributedString(s"   $box ")
  }

}
