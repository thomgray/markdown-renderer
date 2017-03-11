package cucumber.steps

import com.gray.markdown.render.MdRenderer
import com.gray.markdown.{MdCode, MdDocument, MdLinkReference}
import com.gray.string.AttributedString
import cucumber.api.PendingException
import cucumber.util.ExpectedRendering

import scala.util.{Failure, Success, Try}

class RenderingSteps extends BaseSteps {
  import ExpectedRendering._

  var expectedRenderResult: AttributedString = null

  Given("""^an MdDocument exists containing only MdCode$""") { () =>
    holder.document = MdDocument(plainCode)
  }

  Given("""^an MdDocument exists containing a string with an? (unreferenced|referenced|labeled|monadic referenced) link$"""){ (linkType: String) =>
    val link = linkType match {
      case "unreferenced" => stringWithUnreferencedLink
      case "referenced" =>
        holder.linkRefs = MdLinkReference("google link", "www.google.com")
        stringWithReferencedLink
      case "monadic referenced" =>
        holder.linkRefs = MdLinkReference("google", "www.google.com")
        stringWithMonadicReferencedLink
      case "labeled" => stringWithLabeledLink
    }
    holder.document = MdDocument(link)
  }

  Given("""^an MdDocument exists containing a string with a mixture of link styles$"""){ () =>
    holder.document = MdDocument(stringWithMixtureOfLinkStyles)
    holder.linkRefs = List(
      MdLinkReference("fb", "www.facebook.com"),
      MdLinkReference("google", "www.google.com")
    )
    expectedRenderResult = ExpectedRendering(stringWithMixtureOfLinkStyles)
  }

  When("""^I render the document$""") { () =>
    holder.renderResult = Try {
      MdRenderer.render(holder.document, 100, holder.linkRefs)
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

  Then("""^the (unreferenced|referenced|labeled) link has been properly formatted$"""){ (linkStyle: String) =>
    val link = linkStyle match {
      case "unreferenced" => stringWithUnreferencedLink
      case "referenced" => stringWithReferencedLink
      case "labeled" => stringWithLabeledLink
    }
    holder.renderResult shouldBe ExpectedRendering(link)
  }

  Then("""^the result has been properly formatted$"""){ () =>
    println(holder.renderResult)
    holder.renderResult.string shouldBe expectedRenderResult.string
    holder.renderResult shouldBe expectedRenderResult
  }

}
