package com.gray.markdown.produce

import com.gray.markdown._

trait MdFactory {

  def makeMdString(string: String): MdString = MdString(string)
  def makeMdQuote(string: String): MdQuote = MdQuote(string)
}
