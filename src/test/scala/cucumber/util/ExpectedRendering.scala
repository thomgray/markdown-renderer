package cucumber.util

import com.gray.markdown._
import com.gray.markdown.util.MapOne
import com.gray.string.AttributedString
import com.gray.util.ImplicitConversions

import scala.io.AnsiColor

object ExpectedRendering extends ImplicitConversions with AnsiColor {

  val plainCode = MdCode("this is some code", None, MdLocation(0,0))
  val stringWithUnreferencedLink = MdString("go to www.google.com for more information", MdLocation(0,0))
  val stringWithLabeledLink = MdString("go to [google](www.google.com) for more information", MdLocation(0,0))
  val stringWithReferencedLink = MdString("go to [google][google link] for more information", MdLocation(0,0))
  val stringWithMonadicReferencedLink = MdString("go to [google] for more information", MdLocation(0,0))
  val stringWithMixtureOfLinkStyles = MdString("go to [google], [facebook][fb], [github](www.githum.com) or www.wikipedia.com for more", MdLocation(0,0))
  val quote = MdQuote("this is a quote", MdLocation(0,0))

  def apply(mdParagraph: MdParagraph): AttributedString = map(mdParagraph)

  //############### private methods ######################\\

  private val newline = AttributedString("\n")

  private val expectedPlainCodeString = {
    val topBottom = "".padTo(100, ' ') << BLACK_B
    val middle = " this is some code".padTo(100, ' ') << BLACK_B
    topBottom + newline + middle + newline + topBottom
  }

  private val expectedUnreferencedLinkString = AttributedString("go to ") + ("www.google.com" << BLUE) + " for more information"
  private val expectedLabeledLinkString = AttributedString("go to ") + ("google" << BLUE) + " for more information"

  private val expectedQuoteString = (" " << WHITE_B) +  " this is a quote"

  private val expectedLinkMixtureString =
    AttributedString("go to ") +
      ("google" << BLUE) +
      ", " +
      ("facebook" << BLUE) +
      ", " +
      ("github" << BLUE) +
      " or " +
      ("www.wikipedia.com" << BLUE) +
      " for more"


  //######################### MAP ######################\\

  private val map = Map[MdParagraph, AttributedString](
    plainCode -> expectedPlainCodeString,
    quote -> expectedQuoteString,
    stringWithUnreferencedLink -> expectedUnreferencedLinkString,
    stringWithLabeledLink -> expectedLabeledLinkString,
    stringWithReferencedLink -> expectedLabeledLinkString,
    stringWithMixtureOfLinkStyles -> expectedLinkMixtureString
  )

  private def padString(string: String, length: Int) = {
    val toPad = length-string.length
    val padString = "".padTo(toPad, ' ')
    string + padString
  }

}
