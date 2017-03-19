package com.gray.markdown.render.renderingrules

import com.gray.markdown.{MdLinkReference, MdLocation, MdString}
import com.gray.string.AttributedString
import com.gray.string.domain.Format
import com.gray.util.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}

import scala.io.AnsiColor

class MdStringRendererSpec extends FlatSpec with Matchers with ImplicitConversions with AnsiColor {
  val noLocation = MdLocation(0,0)

  val renderer = MdStringRenderer
  protected val UNDERLINED_FORMAT = Format(other = Some(List(AnsiColor.UNDERLINED)))

  def render(mdString: MdString, width: Int, lr: List[MdLinkReference] = Nil) = {
    renderer.render(mdString, width, lr, (_, _, _) => AttributedString("")).getOrElse(AttributedString(""))
  }
  def renderString(mdString: MdString, width: Int, lr: List[MdLinkReference] = Nil) = {
    renderer.render(mdString, width, lr, (_, _, _) => AttributedString("")).getOrElse(AttributedString(""))
  }

  it should "noramlise word spacing" in {
    val str = "this is  an    irregularly\nspaced   string"
    val expected = "this is an irregularly spaced string"

    val mdString = MdString(str, noLocation)
    render(mdString, 100, Nil) shouldBe AttributedString(expected)
  }

  it should "break lines when the line ends in a double space" in {
    val str = "this is a string  \nthat breaks here"
    val expected = "this is a string\nthat breaks here"

    render(str, 100, Nil).string shouldBe expected
  }

  it should "wrap lines to within the specified with" in {
    val str = "look at this lovely string, isn't it beautiful"
    render(str, 20, Nil) shouldBe AttributedString("look at this lovely\nstring, isn't it\nbeautiful")
  }

  it should "apply inline formatting" in {
    val str = MdString("this is __bold__ and this is _underlined_", noLocation)
    val actual = renderString(str, 100)
    val expected = AttributedString("this is ") +
      (AttributedString("bold") << BOLD) +
      AttributedString(" and this is ") +
      (AttributedString("underlined") << UNDERLINED_FORMAT)
    expected shouldBe actual
  }

  it should "highlight urls in blue" in {
    val str: MdString = "go to www.google.com for more"
    val expected = AttributedString("go to ") + ("www.google.com" << BLUE) + " for more"
    val actual = render(str, 100, Nil)
    expected.string shouldBe actual.string
    actual shouldBe expected
  }

  it should "replace labeled urls with properly formatted urls" in {
    val str: MdString = "go to [google](www.google.com) for more"
    val expected = AttributedString("go to ") + ("google" << BLUE) + " for more"
    val actual = render(str, 100, Nil)
    expected shouldBe actual
  }

  it should "replace referenced urls with formatted urls if the reference is specified" in {
    val str: MdString = "go to [google][google link] for more"
    val expected = AttributedString("go to ") + ("google" << BLUE) + " for more"
    val actual = render(str, 100, MdLinkReference("google link", "www.google.com", noLocation))
    expected shouldBe actual
  }

}
