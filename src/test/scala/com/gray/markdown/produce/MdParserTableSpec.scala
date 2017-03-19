package com.gray.markdown.produce

import com.gray.markdown._
import org.scalatest.{FlatSpec, Matchers}

class MdParserTableSpec extends FlatSpec with MdParser with Matchers {

  it should "parse tables" in {
    val str =
      """| col1 | col2 |
        ||------|------|
        ||1     |2     |""".stripMargin

    val pars = parse(str).paragraphs
    pars.length shouldBe 1
    pars.head shouldBe a [MdTable]
  }

}
