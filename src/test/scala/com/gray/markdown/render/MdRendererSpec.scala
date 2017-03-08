package com.gray.markdown.render

import com.gray.markdown._
import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}

import scala.io.AnsiColor

class MdRendererSpec extends FlatSpec with Matchers with MdRenderer with ImplicitConversions with AnsiColor {

  "renderHeader" should "render a v1 header with double underlines" in {
    val header = MdHeader("header", 1)
    val expected = AttributedString("HEADER\n══════") << BOLD
    val actual = render(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  it should "render a v2 header with single underline" in {
    val header = MdHeader("header", 2)
    val expected = AttributedString("HEADER\n──────") << BOLD
    val actual = renderHeader(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  it should "render a v3 header with underlined format" in {
    val header = MdHeader("header", 3)
    val expected = AttributedString("HEADER") << BOLD << stringToFormat(UNDERLINED)
    val actual = renderHeader(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  it should "render a v4 header in capitals" in {
    val header = MdHeader("header", 4)
    val expected = AttributedString("HEADER") << BOLD
    val actual = renderHeader(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  it should "render a v5 header in bold" in {
    val header = MdHeader("header", 5)
    val expected = AttributedString("header") << BOLD
    val actual = renderHeader(header, 50)
    actual.attributes shouldBe expected.attributes
    actual.string shouldBe expected.string
  }

  "renderString" should "noramlise word spacing" in {
    val str = "this is  an    irregularly\nspaced   string"
    val expected = "this is an irregularly spaced string"

    val mdString = MdString(str)
    render(mdString, 100) shouldBe AttributedString(expected)
  }

  it should "break lines when the line ends in a double space" in {
    val str = "this is a string  \nthat breaks here"
    val expected = "this is a string\nthat breaks here"

    render(str, 100).string shouldBe expected
  }

  it should "wrap lines to within the specified with" in {
    val str = "look at this lovely string, isn't it beautiful"
    render(str, 20) shouldBe AttributedString("look at this lovely\nstring, isn't it\nbeautiful")
  }

  it should "apply inline formatting" in {
    val str = MdString("this is __bold__ and this is _underlined_")
    val actual = renderString(str, 100)
    val expected = AttributedString("this is ") +
      (AttributedString("bold") << BOLD) +
      AttributedString(" and this is ") +
      (AttributedString("underlined") << UNDERLINED_FORMAT)
    expected shouldBe actual
  }

  "renderList" should "bullet a list with an indent" in {
    val list = MdBulletList(List(MdBulletListItem(MdString("one")), MdBulletListItem(MdString("two"))))
    val actual = render(list, 100).string
    actual shouldBe
      """   • one
        |   • two""".stripMargin
  }

  it should "indent but not bullet paragraphs within a single bullet" in {
    val list = MdBulletList(MdBulletListItem(List("one", "two", "three")))
    render(list, 100).string shouldBe
      """   • one
        |
        |     two
        |
        |     three""".stripMargin
  }

  it should "indent bullet tiers and choose the appropriate bullet" in {
    val lastList = MdBulletList(MdBulletListItem(MdString("one")))
    val secondList = MdBulletList(MdBulletListItem(List("two", lastList)))
    val list = MdBulletList(MdBulletListItem(List("three", secondList)))

    val actual = render(list, 100).string
    actual shouldBe
      """   • three
        |
        |        ◦ two
        |     |
        |             ⁃ one""".stripMargin.replaceAll("\\|", "")
  }

  it should "render a numbered list" in {
    val list = MdNumberList(MdNumberListItem(MdString("hello")))
    val actual = render(list, 100)

    actual.string shouldBe "  1. hello"
  }

  it should "right align the numerals" in {
    val list = MdNumberList(List(
      MdNumberListItem(MdString("one")),
      MdNumberListItem(MdString("two")),
      MdNumberListItem(MdString("three")),
      MdNumberListItem(MdString("four")),
      MdNumberListItem(MdString("five")),
      MdNumberListItem(MdString("six")),
      MdNumberListItem(MdString("seven")),
      MdNumberListItem(MdString("eight")),
      MdNumberListItem(MdString("nine")),
      MdNumberListItem(MdString("ten"))
    ))

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

    render(list, 50).string shouldBe expected
  }

  it should "render a check list" in {
    val list = MdChecktList(List(
      MdCheckListItem(MdString("checked"), checked = true),
      MdCheckListItem(MdString("unchecked"), checked = false)
    ))

    val actual = render(list, 100).toString

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
    val list = MdBulletList(MdBulletListItem(MdString(str)))

    render(list, 30).string shouldBe expected
  }

  "renderQuote" should "render a quote" in {
    val quote = MdQuote("hello there, this is a quote, don't you know")
    val expected = (AttributedString(" ") << WHITE_B) + AttributedString(" hello there, this is a quote, don't you know")
    render(quote, 100) shouldBe expected
  }


  "renderCode" should "maintain the literal text padding the start, end, top and bottom by a single space" in {
    val codeString =
    """|
       |  def main = {
       |    this will not compile
       |  }
       |""".stripMargin
    val code = MdCode(codeString, None)
    val actual = renderCode(code, 30).string

    val expected =
    """|                              |
       |                              |
       |   def main = {               |
       |     this will not compile    |
       |   }                          |
       |                              |
       |                              |""".stripMargin.replaceAll("\\|", "")
    actual shouldBe expected
  }

  it should "colour the background black" in {
    val code = MdCode("some code", None)
    val actual = renderCode(code, 20)
    val expectedStr =
     """|                    |
        | some code          |
        |                    |""".stripMargin.replaceAll("\\|", "")
    val expected = expectedStr.split("\n").map(AttributedString(_) << BLACK_B).reduce(_+newLine+_)
    actual shouldBe expected
  }
}
