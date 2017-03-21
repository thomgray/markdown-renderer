package com.gray.markdown.render.codecolourers

import com.gray.string.AttributedString
import com.gray.string.domain.Attribute

object MdJsonCodeColourer extends MdCodeColouring {
  val keysRegex = """(?<!\\)(\".+?(?<!\\)\")(?:\s*)\:""".r
  val stringsRegex = """(?<!\\)\".+?(?<!\\)\"""".r

  override def colourCode(string: AttributedString): AttributedString = {
    val keyMatches = keysRegex.findAllMatchIn(string.string).toList

    applyFormatIterating(RED, keyMatches, string)(m => (m.start, m.end - 1)) | { str =>
      "\\d+".r.findAllMatchIn(str.string).foldLeft(str)((s, mtch) => {
        if (!keyMatches.exists(m => m.start < mtch.start && m.end > mtch.end)) {
          s < Attribute(BLUE, mtch.start, mtch.end)
        } else s
      })
    } | { str =>
      val plainStringMatches = stringsRegex.findAllMatchIn(string.string)
      applyFormatConditionally(GREEN, plainStringMatches.toSeq, str) { m =>
        if (keyMatches.exists(keyMatch => keyMatch.start <= m.start && keyMatch.end >= m.end)) None
        else Some((m.start, m.end))
      }
    }

  }
}
