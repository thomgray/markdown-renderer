package com.gray.markdown.produce.parsingrules

import com.gray.markdown.{MdDocument, MdTable}
import org.scalatest.{FlatSpec, Matchers}

class MdTableParsingRuleSpec extends FlatSpec with Matchers {
  import MdTableParsingRule._

  val dudParseFunction: (List[String], Int) => MdDocument = (l, m) => MdDocument(Nil, Nil)

  def parse(string: String) = {
    val lines = string.split("\n").toList
    findParagraph(lines, 0,0,dudParseFunction)
  }

  it should "parse tables" in {
    val str =
    """| col1 | col2 |
      ||------|------|
      ||1     |2     |""".stripMargin

    val pars = parse(str)
    pars shouldBe defined
    pars.get._1 shouldBe a [MdTable]
  }

  it should "read the table headers" in {
    val str =
      """| col1 | col2 |
        ||------|------|
        ||1     |2     |""".stripMargin

    val pars = parse(str)
    pars shouldBe defined
    val table = pars.get._1.asInstanceOf[MdTable]
    table.headers.length shouldBe 2
    table.headers.head.string shouldBe "col1"
    table.headers.last.string shouldBe "col2"
  }

  it should "read the table data" in {
    val str =
      """| col1 | col2 |
        ||------|------|
        ||1     |2     |""".stripMargin

    val pars = parse(str)
    pars shouldBe defined
    val table = pars.get._1.asInstanceOf[MdTable]
    table.data.length shouldBe 1
    val row1 = table.data.head
    row1.data.length shouldBe 2
    row1.data.head.string shouldBe "1"
    row1.data.last.string shouldBe "2"
  }

  it should "read column alignment" in {
    val str =
      """|1 | 2 |3   |
        ||--|--:|:--:|
        ||1 |  2| 3  |
      """.stripMargin
    val pars = parse(str)
    pars shouldBe defined
    val table = pars.get._1.asInstanceOf[MdTable]
    table.columnAlignments shouldBe List(ALIGN_LEFT, ALIGN_RIGHT, ALIGN_CENTRE)
  }

  it should "end when it reaches a blank line" in {
    val str =
      """| col1 | col2 |
        ||------|------|
        ||1     |2     |
        |
        |""".stripMargin

    val pars = parse(str)
    pars shouldBe defined
    pars.get._2 shouldBe 3
  }

  it should "end when it reaches a header" in {
    val str =
      """| col1 | col2 |
        ||------|------|
        ||1     |2     |
        |# header
        |""".stripMargin

    val pars = parse(str)
    pars shouldBe defined
    pars.get._2 shouldBe 3
  }

  it should "end when it reaches a code block" in {
    val str =
      """| col1 | col2 |
        ||------|------|
        ||1     |2     |
        |```
        |code
        |```
        |""".stripMargin

    val pars = parse(str)
    pars shouldBe defined
    pars.get._2 shouldBe 3
  }
}
