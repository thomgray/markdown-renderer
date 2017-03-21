package com.gray.markdown.render.codecolourers
import com.gray.string.AttributedString

object MdBashCodeColourer extends MdCodeColouring {
  override def colourCode(string: AttributedString): AttributedString = {
    applyFormat(COMMENTED, COMMENT_REGEX_HASH, string)
  }
}
