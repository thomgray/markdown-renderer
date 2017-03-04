package com.gray.markdown.render.rendertools

import com.gray.string.AttributedString
import org.scalatest.{FlatSpec, Matchers}

class StringFormattingSpec extends FlatSpec with Matchers with StringFormatting {

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

}
