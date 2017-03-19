package com.gray.markdown.produce

import com.gray.markdown._
import com.gray.util.ImplicitConversions
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class MdParserSpec extends FlatSpec with Matchers with MockitoSugar with BeforeAndAfter with MdRegexes with ImplicitConversions with MdParser {

  import scala.language.implicitConversions

  private val noLocation = @@(0,0)

  val stubCodeBlock = MdCode("foo", None, noLocation)
  val stubString = MdString("foo",noLocation)
  val stubQuote = MdQuote("foo", noLocation)
  val stubBulletItem = MdBulletListItem(Nil, noLocation)
  val stubBulletList = MdBulletList(List(stubBulletItem), noLocation)

  val stringOne = MdString("one", noLocation)
  val stringTwo = MdString("two", noLocation)
  val stringThree = MdString("three", noLocation)
  val stringFour = MdString("four", noLocation)

  it should "parse code" in {
    val str =
      """```
        |this is some code
        |```""".stripMargin


    val result = parse(str).paragraphs
    result shouldBe List(MdCode("this is some code", None, @@(0,3)))
  }

  it should "parse indented code" in {
    val str =
      """    this is indented!
        |    so is this""".stripMargin


    val result = parse(str).paragraphs

    result shouldBe List(MdCode("this is indented!\nso is this", None, @@(0,2)))
  }

  it should "treat unchecked text as a string" in {
    val str =
      """this is some string
        |nothing special about it really!
        |how are you?""".stripMargin

    val result  = parse(str)

    result.paragraphs shouldBe List(MdString(str, @@(0,3)))
  }

  it should "parse quotes" in {
    val str =
      """> this is a quote
        |> accross several lines""".stripMargin

    val result = parse(str).paragraphs
    result.length shouldBe 1
    result.head shouldBe MdQuote("this is a quote accross several lines", @@(0,2))
  }

  it should "parse quotes when preceded by blank space" in {
    val str =
      """
        |> this is a quote
        |""".stripMargin
    val result = parse(str).paragraphs
    result.length shouldBe 1
    result.head shouldBe MdQuote("this is a quote", @@(1,2))
  }

  it should "parse a single bullet list item" in {
    val str = """- hello"""

    val result = parse(str).paragraphs

    result shouldBe List(MdBulletList(List(MdBulletListItem(List(stringToMdString("hello")), @@(0,1))), @@(0,1)))
  }

  it should "parse bulleted lists" in {
    val str =
      """- one
        |- two
        |- three
        |  - four""".stripMargin

    val result = parse(str).paragraphs

    result.length shouldBe 1
    val list = result.head.asInstanceOf[MdBulletList]
    list.location shouldBe @@(0,4)

    val listItems = list.items
    listItems.length shouldBe 3
    listItems(0) shouldBe MdBulletListItem(MdString("one", @@(0,1)), @@(0,1))
    listItems(1) shouldBe MdBulletListItem(MdString("two", @@(1,2)), @@(1,2))
    listItems(2) shouldBe MdBulletListItem(List(
      MdString("three", @@(2,3)),
      MdBulletList(MdBulletListItem(MdString("four", @@(3,4)), @@(3,4)), @@(3,4))
    ), @@(2,4))
  }

  it should "parse asterisk bullets" in {
    val str =
      """* one
        |* two""".stripMargin

    val result= parse(str).paragraphs

    result shouldBe List(MdBulletList(List(
      MdBulletListItem(MdString("one", @@(0,1)),@@(0,1)),
      MdBulletListItem(MdString("two", @@(1,2)), @@(1,2))
    ), @@(0,2)
    ))


  }

  it should "parse plus sign bullets" in {
    val str =
      """+ one
        |+ two""".stripMargin

    val result= parse(str).paragraphs

    result shouldBe List(MdBulletList(List(
      MdBulletListItem(MdString("one", @@(0,1)), @@(0,1)),
      MdBulletListItem(MdString("two", @@(1,2)), @@(1,2))
    ), @@(0,2)))
  }

  it should "break a list when formatting ends" in {
    val str =
      """- one
        |- two
        |hello""".stripMargin

    val results = parse(str).paragraphs

    results.length shouldBe 2
    results(0) shouldBe a [MdBulletList]
    results(1) shouldBe MdString("hello", @@(2,3))
  }

  it should "parse numbered lists" in {
    val str = """1. one"""

    val results = parse(str).paragraphs

    results shouldBe List(
      MdNumberList(MdNumberListItem(MdString("one", @@(0,1)), @@(0,1)), @@(0,1))
    )
  }

  it should "parse task|check lists" in {
    val str = """- [ ] one"""
    parse(str).paragraphs shouldBe List(
      MdChecktList(MdCheckListItem(MdString("one", @@(0,1)), false, @@(0,1)), @@(0,1))
    )
  }

  it should "parse a micture of check list items with different check-values" in {
    val str =
      """- [ ] one
        |- [x] two""".stripMargin
    parse(str).paragraphs shouldBe List(MdChecktList(List(
      MdCheckListItem(MdString("one", @@(0,1)), false, @@(0,1)),
      MdCheckListItem(MdString("two",@@(1,2)), true, @@(1,2))
    ), @@(0,2)))
  }

  it should "parse headers" in {
    val str = """# header1"""

    val results = parse(str).paragraphs

    results shouldBe List(
        MdHeader("header1", 1, @@(0,1))
    )
  }

  it should "parse headers with horizontal lines" in {
    val str =
      """hello
        |---""".stripMargin

    val result = parse(str).paragraphs

    result shouldBe List(
      MdHeader("hello", 2, @@(0,2))
    )
  }

  it should "parse headers with equals lines" in {
    val str =
      """hello
        |===""".stripMargin
    val result = parse(str).paragraphs

    result shouldBe List(
      MdHeader("hello", 1, @@(0,2))
    )
  }

  it should "break paragraphs when there is an empty line" in {
    val str =
      """hello there
        |
        |this is another""".stripMargin
    
    val result = parse(str).paragraphs

    result.length shouldBe 2
    result(0) shouldBe MdString("hello there", @@(0,1))
    result(1) shouldBe MdString("this is another", @@(2,3))
  }

  it should "handle empty strings" in {
    val string =
      """
        |
        |
        |
      """.stripMargin
    parse(string).paragraphs shouldBe Nil
  }

  it should "handle empty space before and afer an item" in {
    parse(
      """
        |this is a string
        |
      """.stripMargin).paragraphs shouldBe List(
      MdString("this is a string", @@(1,2))
    )
  }

}
