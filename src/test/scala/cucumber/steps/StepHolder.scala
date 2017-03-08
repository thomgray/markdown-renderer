package cucumber.steps

import com.gray.markdown.MdDocument
import com.gray.string.AttributedString

object StepHolder {

  var document: MdDocument = null
  var renderResult: AttributedString = null

  var rawString: String = null

}
