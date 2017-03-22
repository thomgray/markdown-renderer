package com.gray.markdown.render.renderingrules

import com.gray.markdown.{MdCode, MdLinkReference, MdLocation, MdString}
import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}

import scala.io.AnsiColor

class MdCodeRendererSpec extends FlatSpec with ImplicitConversions with AnsiColor with Matchers {

  val noLocation = MdLocation(0,0)
  val renderer = MdCodeRenderer
  protected val newLine = AttributedString("\n")

  def renderCode(mdCode: MdCode, width: Int, lr: List[MdLinkReference] = Nil) = {
    renderer.render(mdCode, width, lr, (_, _, _) => AttributedString("")).getOrElse(AttributedString(""))
  }

  it should "maintain the literal text padding the start, end, top and bottom by a single space" in {
    val codeString =
      """|
        |  def main = {
        |    this will not compile
        |  }
        |""".stripMargin
    val code = MdCode(codeString, None, noLocation)
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
    val code = MdCode("some code", None, noLocation)
    val actual = renderCode(code, 20)
    actual.attributes.attributes.foreach(_.format.background shouldBe Some(AnsiColor.BLACK_B))
  }

  it should "colour the foreground white" in {
    val code = MdCode("some code", None, noLocation)
    val actual = renderCode(code, 20)
    val actualString = actual.string
    actual.attributes.attributes.foreach(at => {
      val (s,e) = at.range
      if (actualString.substring(s,e).matches(".*\\w.*")) {
        at.format.foreground shouldBe Some(AnsiColor.WHITE)
      }
    })
  }

}
