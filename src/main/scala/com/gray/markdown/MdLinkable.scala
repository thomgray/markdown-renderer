package com.gray.markdown

import com.gray.markdown.produce.MdRegexes

trait MdLinkable {

  def links(linkContext: List[MdLinkReference] = Nil): List[MdLink]

  def extractLinksWithString(string: String, linkContext: List[MdLinkReference] = Nil): List[MdLink] = {
    def extractLinksRecursive(string: String, soFar: List[MdLink] = Nil): List[MdLink] = {
      MdRegexes.mdLinkRegex.findFirstMatchIn(string) match {
        case Some(mtch) =>
          val newString = string.substring(0, mtch.start) + string.substring(mtch.end)
          extractLinksRecursive(newString, MdLink(mtch.group(2), Some(mtch.group(1))) +: soFar)
        case None =>
          MdRegexes.urlRegex.findFirstIn(string) match {
            case Some(url) =>
              val removeIt = string.replaceAllLiterally(url, "")
              extractLinksRecursive(removeIt, MdLink(url, None) +: soFar)
            case None =>
              MdRegexes.inSquareBracesRegex.findFirstMatchIn(string) match {
                case Some(mtch) if linkContext.exists(_.label==mtch.group(2)) =>
                  val ref = linkContext.find(_.label==mtch.group(2)).get
                  val newString = string.substring(0, mtch.start) + string.substring(mtch.end)
                  val label = Option(mtch.group(1)) match {
                    case Some(group) => group
                    case None => mtch.group(2)
                  }
                  extractLinksRecursive(newString, MdLink(ref.url, Some(label)) +: soFar)
                case _ => soFar
              }
          }
      }
    }
    extractLinksRecursive(string)
  }

}
