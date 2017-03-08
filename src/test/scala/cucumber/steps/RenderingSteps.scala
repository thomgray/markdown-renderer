package cucumber.steps

import com.gray.markdown.render.MdRenderer
import com.gray.markdown.{MdCode, MdDocument}

import scala.util.{Failure, Success, Try}

class RenderingSteps extends BaseSteps {

  Given("""^an MdDocument exists containing only MdCode$""") { () =>
    holder.document = MdDocument(MdCode(
      """
        |def main(args: Array[String]) = {
        |  println("this is a string")
        |}
        |
      """.stripMargin, None))
  }

  When("""^I render the document$""") { () =>
    holder.renderResult = Try{
      MdRenderer.render(holder.document, 100)
    } match {
      case Success(res) => res
      case Failure(e) =>
        println(s">>> rendering failed with exception: ${e.getMessage}")
        e.printStackTrace()
        null
    }
  }

  Then("""^I receive an attributed string$""") { () =>
    holder.renderResult shouldNot be (null)
  }

  Then("""^show me the money$""") { () =>
    println(holder.renderResult)
  }


}
