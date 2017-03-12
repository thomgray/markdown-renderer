package com.gray.markdown

case class MdDocument(paragraphs: List[MdParagraph], linkRefs: List[MdLinkReference] = Nil)

abstract class MdParagraph(val location: MdLocation)

case object MdBreak extends MdParagraph(MdLocation(0,0))

case class MdHeader(mdString: MdString, value: Int, override val location: MdLocation) extends MdParagraph(location) with MdLinkable {
  override def links(linkContext: List[MdLinkReference]): List[MdLink] = mdString.links(linkContext)
}

case class MdString(string: String, override val location: MdLocation) extends MdParagraph(location) with MdLinkable {
  override def links(linkContext: List[MdLinkReference]): List[MdLink] = extractLinksWithString(string, linkContext)
}

case class MdQuote(string: String, override val location: MdLocation) extends MdParagraph(location)

case class MdCode(string: String, language: Option[String], override val location: MdLocation) extends MdParagraph(location)

//---------------------------------------------------------------------------\\
//                           lists                                           \\
//---------------------------------------------------------------------------\\

abstract class MdList[+Item <: MdListItem](val items: List[Item], override val location: MdLocation) extends MdParagraph(location)

abstract class MdListItem(val paragraphs: List[MdParagraph], override val location: MdLocation) extends MdParagraph(location)

object MdList {
  def unapply(arg: MdBulletList): Option[List[MdBulletListItem]] = Some(arg.items)

  def unapply(arg: MdNumberList): Option[List[MdNumberListItem]] = Some(arg.items)

  def unapply(arg: MdChecktList): Option[List[MdCheckListItem]] = Some(arg.items)
}

case class MdNumberList(override val items: List[MdNumberListItem], override val location: MdLocation) extends MdList[MdNumberListItem](items, location)

case class MdNumberListItem(override val paragraphs: List[MdParagraph], override val location: MdLocation) extends MdListItem(paragraphs, location)

case class MdBulletList(override val items: List[MdBulletListItem], override val location: MdLocation) extends MdList[MdBulletListItem](items, location)

case class MdBulletListItem(override val paragraphs: List[MdParagraph], override val location: MdLocation) extends MdListItem(paragraphs, location)

case class MdChecktList(override val items: List[MdCheckListItem], override val location: MdLocation) extends MdList[MdCheckListItem](items, location)

case class MdCheckListItem(override val paragraphs: List[MdParagraph], checked: Boolean, override val location: MdLocation) extends MdListItem(paragraphs, location)

case class MdLink(url: String, label: Option[String])

case class MdLinkReference(label: String, url: String, location: MdLocation)

case class MdLocation(startLine: Int, endLine: Int, startColumn: Int = 0, endColumn: Int = 0)