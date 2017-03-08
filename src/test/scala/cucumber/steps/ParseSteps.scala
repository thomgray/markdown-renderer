package cucumber.steps

import com.gray.markdown.MdChecktList
import com.gray.markdown.produce.MdParser
import com.gray.markdown.render.MdRenderer
import cucumber.api.PendingException

class ParseSteps extends BaseSteps {

  When("""^I parse a string containing a nested check list$""") { () =>
    holder.rawString =
      """- [ ] check this
        |  - [ ] and this
        |""".stripMargin
  }
  Then("""^I receive a document containing (\d+) paragraph$""") { (arg0: Int) =>
    holder.document = MdParser.parse(holder.rawString)
  }

  Then("""^the (\d+)(?:st|nd|rd|th) paragraph is a "([^"]*)"$""") { (arg0: Int, arg1: String) =>
    holder.document.paragraphs(arg0-1) shouldBe (arg1 match {
      case "task list" => a[MdChecktList]
      case _ => throw new PendingException()
    })
  }

  Then("""^the (\d+)st paragraph is a task list containing a single item with a single nested item$""") { (arg0: Int) =>
    val par = holder.document.paragraphs(arg0-1)
    par shouldBe a [MdChecktList]
    val cl = par.asInstanceOf[MdChecktList]

    cl.items.length shouldBe 1
  }


}
