package com.gray.markdown

case class MdDocument(paragraphs: List[MdParagraph])

abstract class MdParagraph

case object MdBreak extends MdParagraph

case class MdHeader(mdString: MdString, value: Int) extends MdParagraph with MdLinkable {
  override def links(linkContext: List[MdLinkReference]): List[MdLink] = mdString.links(linkContext)
}

case class MdString(string: String) extends MdParagraph with MdLinkable {
  override def links(linkContext: List[MdLinkReference]): List[MdLink] = extractLinksWithString(string, linkContext)
}

case class MdQuote(string: String) extends MdParagraph

case class MdCode(string: String, language: Option[String]) extends MdParagraph

//---------------------------------------------------------------------------\\
//                           lists                                           \\
//---------------------------------------------------------------------------\\

abstract class MdList[+Item <: MdListItem](val items: List[Item]) extends MdParagraph

abstract class MdListItem(val paragraphs: List[MdParagraph]) extends MdParagraph

object MdList {
  def unapply(arg: MdBulletList): Option[List[MdBulletListItem]] = Some(arg.items)

  def unapply(arg: MdNumberList): Option[List[MdNumberListItem]] = Some(arg.items)

  def unapply(arg: MdChecktList): Option[List[MdCheckListItem]] = Some(arg.items)
}

case class MdNumberList(override val items: List[MdNumberListItem]) extends MdList[MdNumberListItem](items)

case class MdNumberListItem(override val paragraphs: List[MdParagraph]) extends MdListItem(paragraphs)

case class MdBulletList(override val items: List[MdBulletListItem]) extends MdList[MdBulletListItem](items)

case class MdBulletListItem(override val paragraphs: List[MdParagraph]) extends MdListItem(paragraphs)

case class MdChecktList(override val items: List[MdCheckListItem]) extends MdList[MdCheckListItem](items)

case class MdCheckListItem(override val paragraphs: List[MdParagraph], checked: Boolean) extends MdListItem(paragraphs)

case class MdLink(url: String, label: Option[String])
case class MdLinkReference(label: String, url: String)
