package cucumber.steps

import com.gray.markdown.{MdDocument, MdLinkReference}
import com.gray.string.AttributedString

object StepHolder {

  def reset = {
    document = null
    renderResult = null
    rawString = null
    linkRefs = Nil
  }

  var document: MdDocument = null
  var renderResult: AttributedString = null

  var rawString: String = null
  var linkRefs: List[MdLinkReference] = Nil

}
