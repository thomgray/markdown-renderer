package com.gray.markdown.produce

import com.gray.markdown._
import com.gray.util.ImplicitConversions
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class MdParserSpec extends FlatSpec with Matchers with MockitoSugar with BeforeAndAfter with MdRegexes with ImplicitConversions with MdParser {

  import scala.language.implicitConversions

  val stubCodeBlock = MdCode("foo", None)
  val stubString = MdString("foo")
  val stubQuote = MdQuote("foo")
  val stubBulletItem = MdBulletListItem(Nil)
  val stubBulletList = MdBulletList(List(stubBulletItem))

  val stringOne = MdString("one")
  val stringTwo = MdString("two")
  val stringThree = MdString("three")
  val stringFour = MdString("four")

  it should "parse code" in {
    val str =
      """```
        |this is some code
        |```""".stripMargin


    val result = parse(str).paragraphs
    result shouldBe List(MdCode("this is some code", None))
  }

  it should "parse indented code" in {
    val str =
      """    this is indented!
        |    so is this""".stripMargin


    val result = parse(str).paragraphs

    result shouldBe List(MdCode("this is indented!\nso is this", None))
  }

  it should "treat unchecked text as a string" in {
    val str =
      """this is some string
        |nothing special about it really!
        |how are you?""".stripMargin

    val result  = parse(str)

    result.paragraphs shouldBe List(MdString(str))
  }

  it should "parse quotes" in {
    val str =
      """> this is a quote
        |> accross several lines""".stripMargin

    val result = parse(str).paragraphs
  }

  it should "parse a single bullet list item" in {
    val str = """- hello"""

    val result = parse(str).paragraphs

    result shouldBe List(MdBulletList(List(MdBulletListItem(List("hello")))))
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
        MdBulletListItem(List(stringOne)),
        MdBulletListItem(List(stringTwo)),
        MdBulletListItem(List(stringThree, MdBulletList(List(MdBulletListItem(List(stringFour))))))
      ))
    )
  }

  it should "parse asterisk bullets" in {
    val str =
      """* one
        |* two""".stripMargin

    val result= parse(str).paragraphs

    result shouldBe List(MdBulletList(List(MdBulletListItem(stringOne), MdBulletListItem(stringTwo))))


  }

  it should "parse plus sign bullets" in {
    val str =
      """+ one
        |+ two""".stripMargin

    val result= parse(str).paragraphs

    result shouldBe List(MdBulletList(List(MdBulletListItem(stringOne), MdBulletListItem(stringTwo))))
  }

  it should "break a list when formatting ends" in {
    val str =
      """- one
        |- two
        |hello""".stripMargin

    val results = parse(str).paragraphs

    results.length shouldBe 2
    results(0) shouldBe a [MdBulletList]
    results(1) shouldBe MdString("hello")
  }

  it should "parse numbered lists" in {
    val str = """1. one"""

    val results = parse(str).paragraphs

    results shouldBe List(
      MdNumberList(MdNumberListItem(stringOne))
    )
  }

  it should "parse headers" in {
    val str = """# header1"""

    val results = parse(str).paragraphs

    results shouldBe List(
        MdHeader("header1", 1)
    )
  }

  it should "parse headers with horizontal lines" in {
    val str =
      """hello
        |---""".stripMargin

    val result = parse(str).paragraphs

    result shouldBe List(
      MdHeader("hello", 2)
    )
  }

  it should "parse headers with equals lines" in {
    val str =
      """hello
        |===""".stripMargin
    val result = parse(str).paragraphs

    result shouldBe List(
      MdHeader("hello", 1)
    )
  }

  it should "break paragraphs when there is a double space" in {
    val str =
      """hello there
        |
        |this is another""".stripMargin
    
    val result = parse(str).paragraphs

    result.length shouldBe 2
    result(0) shouldBe MdString("hello there")
    result(1) shouldBe MdString("this is another")
  }


  "findListItem" should "pick out a bullet list item body" in {
    val str = """- hello there"""
    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      List("hello there"), 1
    )
  }

  it should "pick out the body accross several lines" in {
    val str =
      """- hello there
        |  and this is another line""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      List("hello there","and this is another line"), 2
    )
  }

  it should "maintain excess indentation" in {
    val str =
      """- hello there
        |      and this is indented""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      List("hello there","    and this is indented"), 2
    )
  }

  it should "maintain text separated by a single line" in {
    val str =
      """- hello there
        |
        |  this is still in the same thing""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      List("hello there", "", "this is still in the same thing"), 3
    )
  }

  it should "pick treat inner lists as well" in {
    val str =
      """- hello there
        |  - inner list""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      List("hello there","- inner list"), 2
    )
  }


}
