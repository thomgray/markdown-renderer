package cucumber.steps

import com.gray.markdown.MdDocument
import com.gray.string.AttributedString

object StepHolder {

  def reset = {
    document = null
    renderResult = null
    rawString = null
  }

  var document: MdDocument = null
  var renderResult: AttributedString = null

  var rawString: String = null

}
