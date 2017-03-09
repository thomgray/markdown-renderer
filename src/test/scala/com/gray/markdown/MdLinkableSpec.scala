package com.gray.markdown

import org.scalatest.{FlatSpec, Matchers}

class MdLinkableSpec extends FlatSpec with MdLinkable with Matchers {
  override def links(linkContext: List[MdLink]): List[MdLink] = Nil

  "extractLinksWithString" should "get unreferenced links" in {
    extractLinksWithString("hello this is a www.google.com link") shouldBe List(MdLink("www.google.com", None))
  }

}
