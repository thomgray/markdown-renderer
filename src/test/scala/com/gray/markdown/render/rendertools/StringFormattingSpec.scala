package com.gray.markdown.render.rendertools

import com.gray.markdown.MdString
import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}

import scala.io.AnsiColor

class StringFormattingSpec extends FlatSpec with Matchers with StringFormatting with ImplicitConversions with AnsiColor{

  "drawBoxAround" should "draw a box around text" in {
    val st =
      """this is a string
        |it is rather fetching
        |that's what I think""".stripMargin

    val as = AttributedString(st)
    drawBoxAround(as).string shouldBe
      """┌─────────────────────┐
        |│this is a string     │
        |│it is rather fetching│
        |│that's what I think  │
        |└─────────────────────┘""".stripMargin
  }

  "applyInlineFormatting" should "make __this__ bold" in {
    val as = AttributedString("make __this__ bold")
    val bold = applyMdInlineFormatting(as)
    bold shouldBe AttributedString("make ") + (AttributedString("this") << AnsiColor.BOLD) + AttributedString(" bold")
  }

  it should "make _this_ underlined" in {
    val as = AttributedString("make _this_ underlined")
    val bold = applyMdInlineFormatting(as)
    bold shouldBe AttributedString("make ") + (AttributedString("this") << AnsiColor.UNDERLINED) + AttributedString(" underlined")
  }

  "applyLabeledLinkHighlighting" should "remove the referenced link and brackets" in {
    val string = "go to [google](www.google.com) for more information"
    val actual = applyLabeledLinkHighlighting(string)
    val expected = AttributedString("go to ") + ("google" << BLUE) + " for more information"
    actual shouldBe expected
  }

  it should "avoid spanning several links" in {
    val string = "blah [google](www.google.com) and [facebook](www.facebook.com) blah"
    val actual = applyLabeledLinkHighlighting(string)
    val expected = AttributedString("blah ") +
        ("google" << BLUE) +
        " and " +
        ("facebook" << BLUE) +
        " blah"
    actual shouldBe expected
  }

}
