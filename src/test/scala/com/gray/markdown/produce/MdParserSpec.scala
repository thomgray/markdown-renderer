package com.gray.markdown.produce

import com.gray.markdown._
import com.gray.util.ImplicitConversions
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class MdParserSpec extends FlatSpec with Matchers with MockitoSugar with BeforeAndAfter with MdRegexes with ImplicitConversions with MdParser {

  import scala.language.implicitConversions

  private val noLocation = MdLocation(0,0)

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
    result shouldBe List(MdCode("this is some code", None, noLocation))
  }

  it should "parse indented code" in {
    val str =
      """    this is indented!
        |    so is this""".stripMargin


    val result = parse(str).paragraphs

    result shouldBe List(MdCode("this is indented!\nso is this", None, noLocation))
  }

  it should "treat unchecked text as a string" in {
    val str =
      """this is some string
        |nothing special about it really!
        |how are you?""".stripMargin

    val result  = parse(str)

    result.paragraphs shouldBe List(MdString(str, noLocation))
  }

  it should "parse quotes" in {
    val str =
      """> this is a quote
        |> accross several lines""".stripMargin

    val result = parse(str).paragraphs
    result.length shouldBe 1
    result.head shouldBe MdQuote("this is a quote accross several lines", noLocation)
  }

  it should "parse quotes when preceded by blank space" in {
    val str =
      """
        |> this is a quote
        |""".stripMargin
    val result = parse(str).paragraphs
    result.length shouldBe 1
    result.head shouldBe MdQuote("this is a quote", noLocation)
  }

  it should "parse a single bullet list item" in {
    val str = """- hello"""

    val result = parse(str).paragraphs

    result shouldBe List(MdBulletList(List(MdBulletListItem(List("hello"), noLocation)), noLocation))
  }

  it should "parse bulleted lists" in {
    val str =
      """- one
        |- two
        |- three
        |  - four""".stripMargin

    val result = parse(str).paragraphs

    result.length shouldBe 1

    result shouldBe List(
      MdBulletList(List(
        MdBulletListItem(List(stringOne), noLocation),
        MdBulletListItem(List(stringTwo), noLocation),
        MdBulletListItem(List(
          stringThree,
          MdBulletList(List(
            MdBulletListItem(List(stringFour),noLocation)
          ),noLocation)
        ),noLocation)
      ), noLocation)
    )
  }

  it should "parse asterisk bullets" in {
    val str =
      """* one
        |* two""".stripMargin

    val result= parse(str).paragraphs

    result shouldBe List(MdBulletList(List(MdBulletListItem(stringOne, noLocation), MdBulletListItem(stringTwo, noLocation)), noLocation))


  }

  it should "parse plus sign bullets" in {
    val str =
      """+ one
        |+ two""".stripMargin

    val result= parse(str).paragraphs

    result shouldBe List(MdBulletList(List(MdBulletListItem(stringOne, noLocation), MdBulletListItem(stringTwo, noLocation)), noLocation))
  }

  it should "break a list when formatting ends" in {
    val str =
      """- one
        |- two
        |hello""".stripMargin

    val results = parse(str).paragraphs

    results.length shouldBe 2
    results(0) shouldBe a [MdBulletList]
    results(1) shouldBe MdString("hello", noLocation)
  }

  it should "parse numbered lists" in {
    val str = """1. one"""

    val results = parse(str).paragraphs

    results shouldBe List(
      MdNumberList(MdNumberListItem(stringOne, noLocation), noLocation)
    )
  }

  it should "parse task|check lists" in {
    val str = """- [ ] one"""
    parse(str).paragraphs shouldBe List(
      MdChecktList(MdCheckListItem(MdString("one", noLocation), false, noLocation), noLocation)
    )
  }

  it should "parse a micture of check list items with different check-values" in {
    val str =
      """- [ ] one
        |- [x] two""".stripMargin
    parse(str).paragraphs shouldBe List(MdChecktList(List(
      MdCheckListItem(MdString("one", noLocation), false, noLocation),
      MdCheckListItem(MdString("two",noLocation), true, noLocation)
    ),noLocation))
  }

  it should "parse headers" in {
    val str = """# header1"""

    val results = parse(str).paragraphs

    results shouldBe List(
        MdHeader("header1", 1, noLocation)
    )
  }

  it should "parse headers with horizontal lines" in {
    val str =
      """hello
        |---""".stripMargin

    val result = parse(str).paragraphs

    result shouldBe List(
      MdHeader("hello", 2, noLocation)
    )
  }

  it should "parse headers with equals lines" in {
    val str =
      """hello
        |===""".stripMargin
    val result = parse(str).paragraphs

    result shouldBe List(
      MdHeader("hello", 1, noLocation)
    )
  }

  it should "break paragraphs when there is an empty line" in {
    val str =
      """hello there
        |
        |this is another""".stripMargin
    
    val result = parse(str).paragraphs

    result.length shouldBe 2
    result(0) shouldBe MdString("hello there", noLocation)
    result(1) shouldBe MdString("this is another", noLocation)
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
      MdString("this is a string", noLocation)
    )
  }


  "findListItem" should "pick out a bullet list item body" in {
    val str = """- hello there"""
    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there"), "- "), 1
    )
  }

  it should "pick out the body accross several lines" in {
    val str =
      """- hello there
        |  and this is another line""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there","and this is another line"), "- "), 2
    )
  }

  it should "maintain excess indentation" in {
    val str =
      """- hello there
        |      and this is indented""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there","    and this is indented"), "- "), 2
    )
  }

  it should "maintain text separated by a single line" in {
    val str =
      """- hello there
        |
        |  this is still in the same thing""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there", "", "this is still in the same thing"), "- "), 3
    )
  }

  it should "pick treat inner lists as well" in {
    val str =
      """- hello there
        |  - inner list""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there","- inner list"), "- "), 2
    )
  }

}
