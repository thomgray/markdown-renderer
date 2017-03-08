package com.gray.markdown.render.rendertools

import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}

import scala.io.AnsiColor

class StringFormattingSpec extends FlatSpec with Matchers with StringFormatting with ImplicitConversions {

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

}
