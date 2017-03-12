package cucumber.steps

import com.gray.markdown.render.MdRenderer
import com.gray.markdown.{MdDocument, MdLinkReference, MdLocation}
import com.gray.string.AttributedString
import cucumber.util.ExpectedRendering

import scala.util.{Failure, Success, Try}

class RenderingSteps extends BaseSteps {
  import ExpectedRendering._

  var expectedRenderResult: AttributedString = null

  Given("""^an MdDocument exists containing only MdCode$""") { () =>
    holder.document = MdDocument(plainCode)
  }

  Given("""^an MdDocument exists containing a string with an? (unreferenced|referenced|labeled|monadic referenced) link$"""){ (linkType: String) =>

    holder.document = linkType match {
      case "unreferenced" => MdDocument(stringWithUnreferencedLink)
      case "referenced" =>
        MdDocument(stringWithReferencedLink, MdLinkReference("google link", "www.google.com", MdLocation(0,0)))
      case "monadic referenced" =>
        MdDocument(stringWithMonadicReferencedLink, MdLinkReference("google", "www.google.com", MdLocation(0,0)))
      case "labeled" => MdDocument(stringWithLabeledLink)
    }
  }

  Given("""^an MdDocument exists containing a string with a mixture of link styles$"""){ () =>
    holder.document = MdDocument(stringWithMixtureOfLinkStyles,
      List(
        MdLinkReference("fb", "www.facebook.com", MdLocation(0,0)),
        MdLinkReference("google", "www.google.com", MdLocation(0,0))
      )
    )
    expectedRenderResult = ExpectedRendering(stringWithMixtureOfLinkStyles)
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
    holder.renderResult shouldBe an [AttributedString]
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
    holder.renderResult.string shouldBe expectedRenderResult.string
    holder.renderResult shouldBe expectedRenderResult
  }

}
