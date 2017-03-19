package com.gray.markdown.produce.parsingrules

import org.scalatest.{FlatSpec, Matchers}

class MdListParsingRuleSpec extends FlatSpec with Matchers {

  import MdListParsingRule._

  "findListItem" should "pick out a bullet list item body" in {
    val str = """- hello there"""
    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there"), "- ", 1)
    )
  }

  it should "pick out the body accross several lines" in {
    val str =
      """- hello there
        |  and this is another line""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there","and this is another line"), "- ", 2)
    )
  }

  it should "maintain excess indentation" in {
    val str =
      """- hello there
        |      and this is indented""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there","    and this is indented"), "- ", 2)
    )
  }

  it should "maintain text separated by a single line" in {
    val str =
      """- hello there
        |
        |  this is still in the same thing""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there", "", "this is still in the same thing"), "- ", 3)
    )
  }

  it should "pick treat inner lists as well" in {
    val str =
      """- hello there
        |  - inner list""".stripMargin

    val lines = findListItem(str.split("\n").toList, 0, bulletListItemRegex)
    lines shouldBe Some(
      (List("hello there","- inner list"), "- ", 2)
    )
  }

}
