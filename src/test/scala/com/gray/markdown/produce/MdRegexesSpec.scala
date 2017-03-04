package com.gray.markdown.produce

import org.scalatest.{FlatSpec, Matchers}

class MdRegexesSpec extends FlatSpec with Matchers with MdRegexes {

  "bulletListItemRegex" should "capture a dashed bullet list item" in {
    val mtch = bulletListItemRegex.findFirstMatchIn("- one").get
    mtch.group(0) shouldBe "- one"
    mtch.group(1) shouldBe "- "
    mtch.group(2) shouldBe "one"
  }

  it should "capture a plus sign bullet list item" in {
    val mtch = bulletListItemRegex.findFirstMatchIn("+ one").get
    mtch.group(0) shouldBe "+ one"
    mtch.group(1) shouldBe "+ "
    mtch.group(2) shouldBe "one"
  }

  it should "capture a asterisk bullet list item" in {
    val mtch = bulletListItemRegex.findFirstMatchIn("* one").get
    mtch.group(0) shouldBe "* one"
    mtch.group(1) shouldBe "* "
    mtch.group(2) shouldBe "one"
  }

  "headerRegex" should "match a header" in {
    headerRegex.findFirstIn("# header") shouldBe defined
  }

  it should "capture the header string and the hashed" in {
    headerRegex.findFirstMatchIn("## header") match {
      case None => fail(s"regex '$headerRegex' did not match '## header'")
      case Some(mtch) =>
        mtch.group(1) shouldBe "##"
        mtch.group(2) shouldBe "header"
    }
  }

}
