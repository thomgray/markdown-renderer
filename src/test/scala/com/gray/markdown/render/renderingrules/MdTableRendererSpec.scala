package com.gray.markdown.render.renderingrules

import com.gray.markdown._
import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}
import sun.security.util.PendingException

class MdTableRendererSpec extends FlatSpec with ImplicitConversions with Matchers {

  import MdTableRenderer._
  val nowhere = @@(0,0)

  val s1 = MdString("1", nowhere)
  val s2 = MdString("2", nowhere)
  val s3 = MdString("3", nowhere)
  val s4 = MdString("4", nowhere)

  val sOne = MdString("one", nowhere)
  val sTwo = MdString("two", nowhere)
  val sThree = MdString("three", nowhere)
  val sFour = MdString("four", nowhere)

  val table1 = MdTable(List(sOne, sTwo),List(MdTableRow(List(s1, s2))), List(0,0), nowhere)

  val renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString = (p, _, _) => p match {
    case MdString(s, _) => AttributedString(s)
    case _ => throw new PendingException()
  }

  it should "draw a box around the headers" in {
    val res = render(table1, 100, Nil, renderer).getOrElse(fail)
    val expectedHeaders =
      """┌─────┬─────┐
        |│ one │ two │
        |╞═════╪═════╡""".stripMargin
    res.string should startWith (expectedHeaders)
  }

  "drawTableBoxes" should "draw boxes around rows" in {
    val r1 = List(
      AttributedString("  1  "), AttributedString("  2  ")
    )
    val r2 = List(
      AttributedString(" one "), AttributedString(" two ")
    )
    val lines = drawTableBoxes(List(r1,r2))
    lines.map(_.string).mkString("\n") shouldBe
      """┌─────┬─────┐
        |│  1  │  2  │
        |╞═════╪═════╡
        |│ one │ two │
        |└─────┴─────┘""".stripMargin
  }

  "rowLine" should "work as expected" in {
    rowLine('┌', '─','┬','┐', List(2,2,3)).string shouldBe "┌──┬──┬───┐"
  }
}
