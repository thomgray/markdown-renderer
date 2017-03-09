package cucumber.steps

import com.gray.markdown.render.MdRenderer
import com.gray.markdown.{MdCode, MdDocument}
import com.gray.string.AttributedString
import cucumber.util.ExpectedRendering

import scala.util.{Failure, Success, Try}

class RenderingSteps extends BaseSteps {
  import ExpectedRendering._

  Given("""^an MdDocument exists containing only MdCode$""") { () =>
    holder.document = MdDocument(plainCode)
  }

  When("""^I render the document$""") { () =>
    holder.renderResult = Try {
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
    holder.renderResult shouldBe a[AttributedString]
    holder.renderResult should not be null
  }

  Then("""^show me the money$""") { () =>
    println(holder.renderResult)
  }

  Then("""^the result is properly formatted code$""") { () =>
    holder.renderResult shouldBe ExpectedRendering(plainCode)
  }

}
