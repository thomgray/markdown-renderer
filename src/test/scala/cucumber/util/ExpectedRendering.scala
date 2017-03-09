package cucumber.util

import com.gray.markdown._
import com.gray.markdown.util.MapOne
import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions

import scala.io.AnsiColor

object ExpectedRendering extends ImplicitConversions with AnsiColor {

  val plainCode = MdCode("this is some code", None)
  val quote = MdQuote("this is a quote")

  def apply(mdParagraph: MdParagraph): AttributedString = map(mdParagraph)

  //############### private methods ######################\\

  private val newline = AttributedString("\n")

  private val expectedPlainCodeString = {
    val topBottom = "".padTo(100, ' ') << BLACK_B
    val middle = " this is some code".padTo(100, ' ') << BLACK_B
    topBottom + newline + middle + newline + topBottom
  }

  private val expectedQuoteString = {
    val frontBit = " " << WHITE_B
    frontBit + " this is a quote"
  }

  //######################### MAP ######################\\

  private val map = Map[MdParagraph, AttributedString](
    plainCode -> expectedPlainCodeString,
    quote -> expectedQuoteString
  )


  private def padString(string: String, length: Int) = {
    val toPad = length-string.length
    val padString = "".padTo(toPad, ' ')
    string + padString
  }

}
