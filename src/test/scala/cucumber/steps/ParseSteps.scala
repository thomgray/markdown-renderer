package cucumber.steps

import com.gray.markdown.produce.MdParser
import com.gray.markdown.{MdChecktList, MdLink, MdLinkable, MdString}
import cucumber.api.PendingException

class ParseSteps extends BaseSteps with MdParser {

  Before { f =>
    holder.reset
  }

  When("""^I parse a string containing a nested check list$""") { () =>
    holder.rawString =
      """- [ ] check this
        |  - [ ] and this
        | """.stripMargin
  }

  When("""^I parse a string containing an? "([^"]*)" link$""") { (arg0: String) =>
    holder.rawString = arg0 match {
      case "unreferenced" => "for more information go to www.google.com"
      case _ => throw new PendingException()
    }
  }

  Then("""^I receive a document containing (\d+) paragraph$""") { (arg0: Int) =>
    holder.document = parse(holder.rawString)
  }

  Then("""^the (\d+)(?:st|nd|rd|th) paragraph is a "([^"]*)"$""") { (arg0: Int, arg1: String) =>
    holder.document.paragraphs(arg0 - 1) shouldBe (arg1 match {
      case "task list" => a[MdChecktList]
      case "string" => a[MdString]
      case _ => throw new PendingException()
    })
  }

  Then("""^the (\d+)st paragraph is a task list containing a single item with a single nested item$""") { (arg0: Int) =>
    val par = holder.document.paragraphs(arg0 - 1)
    par shouldBe a[MdChecktList]
    val cl = par.asInstanceOf[MdChecktList]

    cl.items.length shouldBe 1
  }

  Then("""^the (\d+)st paragraph contains an? "([^"]*)" link$""") { (arg0: Int, arg1: String) =>
    val par = holder.document.paragraphs(arg0 - 1)
    par.asInstanceOf[MdLinkable].links() shouldBe (arg1 match {
      case "unreferenced" =>
        List(
          MdLink("www.google.com", None)
        )
      case _ => throw new PendingException()
    })
  }


}
