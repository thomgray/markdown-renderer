package com.gray.markdown

import org.scalatest.{FlatSpec, Matchers}

class MdLinkableSpec extends FlatSpec with MdLinkable with Matchers {
  override def links(linkContext: List[MdLinkReference]): List[MdLink] = Nil
  val noLocation = MdLocation(0,0)

  "extractLinksWithString" should "get unreferenced links" in {
    extractLinksWithString("hello this is a www.google.com link") shouldBe List(MdLink("www.google.com", None))
  }

  it should "find referenced links" in {
    extractLinksWithString("go to [wikipedia](http://www.wikipedia.com) for more details") shouldBe List(
      MdLink("http://www.wikipedia.com", Some("wikipedia"))
    )
  }

  it should "find a referenced link given the reference exists in that context" in {
    val context = List(
      MdLinkReference("google", "www.google.com", noLocation)
    )
    extractLinksWithString("go to [google] for more info", context) shouldBe List(
      MdLink("www.google.com", Some("google"))
    )
  }

  it should "find a referenced link in the appropriate context with a different label" in {
    val context = List(
      MdLinkReference("google link", "www.google.com", noLocation)
    )
    extractLinksWithString("go to [google][google link]", context) shouldBe List(
      MdLink("www.google.com", Some("google"))
    )
  }

}
