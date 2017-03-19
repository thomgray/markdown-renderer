package com.gray.markdown.render.renderingrules

import com.gray.markdown.{MdHeader, MdLinkReference, MdLocation, MdParagraph}
import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}

import scala.io.AnsiColor

class MdHeaderRendererSpec extends FlatSpec with Matchers with ImplicitConversions with AnsiColor {

  val renderer = MdHeaderRenderer
  val dudRenderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString = (_,_,_) => AttributedString("header")

  val noLocation = MdLocation(0,0)

  def render(mdHeader: MdHeader, width: Int, linkRefs: List[MdLinkReference] = Nil) = {
    renderer.render(mdHeader,
      width,
      linkRefs,
      dudRenderer
    ).getOrElse(fail)
  }

  def renderHeader(mdHeader: MdHeader, width: Int, linkRefs: List[MdLinkReference] = Nil) = {
    renderer.render(mdHeader,
      width,
      linkRefs,
      dudRenderer
    ).getOrElse(fail)
  }

  it should "render a v1 header with double underlines" in {
    val header = MdHeader("header", 1, noLocation)
    val expected = AttributedString("HEADER\n══════") << BOLD
    val actual = render(header, 50, Nil)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  it should "render a v2 header with single underline" in {
    val header = MdHeader("header", 2, noLocation)
    val expected = AttributedString("HEADER\n──────") << BOLD
    val actual = renderHeader(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  it should "render a v3 header with underlined format" in {
    val header = MdHeader("header", 3, noLocation)
    val expected = AttributedString("HEADER") << BOLD << stringToFormat(UNDERLINED)
    val actual = renderHeader(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  it should "render a v4 header in capitals" in {
    val header = MdHeader("header", 4, noLocation)
    val expected = AttributedString("HEADER") << BOLD
    val actual = renderHeader(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  it should "render a v5 header in bold" in {
    val header = MdHeader("header", 5, noLocation)
    val expected = AttributedString("header") << BOLD
    val actual = renderHeader(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

}
