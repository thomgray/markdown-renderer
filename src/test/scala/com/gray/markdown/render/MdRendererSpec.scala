package com.gray.markdown.render

import com.gray.markdown._
import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}

import scala.io.AnsiColor

class MdRendererSpec extends FlatSpec with Matchers with MdRenderer with ImplicitConversions with AnsiColor {

  val noLocation = MdLocation(0,0)


  "renderList" should "bullet a list with an indent" in {
    val list = MdBulletList(List(
      MdBulletListItem(MdString("one", noLocation), noLocation), MdBulletListItem(MdString("two", noLocation), noLocation)
    ), noLocation)
    val actual = render(list, 100, MdLinkReference("google link", "www.google.com",noLocation)).string
    actual shouldBe
      """   • one
        |   • two""".stripMargin
  }

  it should "indent but not bullet paragraphs within a single bullet" in {
    val list = MdBulletList(MdBulletListItem(List("one", "two", "three"), noLocation), noLocation)
    render(list, 100, Nil).string shouldBe
      """   • one
        |
        |     two
        |
        |     three""".stripMargin
  }

  it should "indent bullet tiers and choose the appropriate bullet" in {
    val lastList = MdBulletList(MdBulletListItem(MdString("one", noLocation), noLocation), noLocation)
    val secondList = MdBulletList(MdBulletListItem(List("two", lastList), noLocation), noLocation)
    val list = MdBulletList(MdBulletListItem(List("three", secondList), noLocation), noLocation)

    val actual = render(list, 100, Nil).string
    actual shouldBe
      """   • three
        |
        |        ◦ two
        |     |
        |             ⁃ one""".stripMargin.replaceAll("\\|", "")
  }

  it should "render a numbered list" in {
    val list = MdNumberList(MdNumberListItem(MdString("hello",noLocation), noLocation), noLocation)
    val actual = render(list, 100, Nil)

    actual.string shouldBe "  1. hello"
  }

  it should "right align the numerals" in {
    val list = MdNumberList(List(
      MdNumberListItem(MdString("one", noLocation), noLocation),
      MdNumberListItem(MdString("two", noLocation),noLocation),
      MdNumberListItem(MdString("three", noLocation),noLocation),
      MdNumberListItem(MdString("four", noLocation),noLocation),
      MdNumberListItem(MdString("five", noLocation),noLocation),
      MdNumberListItem(MdString("six", noLocation),noLocation),
      MdNumberListItem(MdString("seven", noLocation),noLocation),
      MdNumberListItem(MdString("eight", noLocation),noLocation),
      MdNumberListItem(MdString("nine", noLocation),noLocation),
      MdNumberListItem(MdString("ten", noLocation),noLocation)
    ), noLocation)

    val expected =
      """  1. one
        |  2. two
        |  3. three
        |  4. four
        |  5. five
        |  6. six
        |  7. seven
        |  8. eight
        |  9. nine
        | 10. ten""".stripMargin

    render(list, 50, Nil).string shouldBe expected
  }

  it should "render a check list" in {
    val list = MdChecktList(List(
      MdCheckListItem(MdString("checked", noLocation), checked = true,noLocation),
      MdCheckListItem(MdString("unchecked", noLocation), checked = false, noLocation)
    ), noLocation)

    val actual = render(list, 100, Nil).toString

    val expected =
      """   ☒ checked
        |   ☐ unchecked""".stripMargin

    actual shouldBe expected
  }

  it should "wrap paragraphs to the correct indent" in {
    val str = """this is a lovely string which is rather long"""
    val expected =
      """   • this is a lovely string
        |     which is rather long""".stripMargin
    val list = MdBulletList(MdBulletListItem(MdString(str, noLocation), noLocation), noLocation)

    render(list, 30, Nil).string shouldBe expected
  }

  "renderQuote" should "render a quote" in {
    val quote = MdQuote("hello there, this is a quote, don't you know", noLocation)
    val expected = (AttributedString(" ") << WHITE_B) + AttributedString(" hello there, this is a quote, don't you know")
    render(quote, 100, Nil) shouldBe expected
  }

}
