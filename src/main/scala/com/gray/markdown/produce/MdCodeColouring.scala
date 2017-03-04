package com.gray.markdown.produce

import com.gray.string.AttributedString

trait MdCodeColouring {

  def colourCode(string: String, language: Option[String]): AttributedString = AttributedString(string)

}
