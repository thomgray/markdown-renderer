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

}
