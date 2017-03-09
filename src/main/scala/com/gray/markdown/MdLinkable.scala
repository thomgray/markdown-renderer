package com.gray.markdown

import com.gray.markdown.produce.MdRegexes

trait MdLinkable {

  private val regex = MdRegexes.urlRegex

  def links(linkContext: List[MdLink] = Nil): List[MdLink]

  def extractLinksWithString(string: String, linkContext: List[MdLink] = Nil): List[MdLink] = {
    def extractLinksRecursive(string: String, soFar: List[MdLink] = Nil): List[MdLink] = {
      regex.findFirstIn(string) match  {
        case Some(url) =>
          val removeIt = string.replaceFirst(url, "")
          extractLinksRecursive(removeIt, MdLink(url, None) +: soFar)
        case None => soFar
      }
    }
    extractLinksRecursive(string)
    ???
  }

}
