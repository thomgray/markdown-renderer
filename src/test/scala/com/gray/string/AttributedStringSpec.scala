package com.gray.string

import com.gray.string.domain.{Attribute, AttributeList, Format}
import org.scalatest.{FlatSpec, Matchers}
import com.gray.util.ImplicitConversions

import scala.io.AnsiColor

class AttributedStringSpec extends FlatSpec with Matchers with AnsiColor with ImplicitConversions {

  val unformattedString = AttributedString("unformatted string")
  val redString = AttributedString("red string", RED)

  def %(string: String) = AttributedString(string)

  "toString" should "write a regular string when the string has no format" in {
    unformattedString.toString shouldBe "unformatted string"
  }

  it should "apply a single attribute" in {
    redString.toString shouldBe s"${RED}red string$RESET"
  }

  it should "apply to a subsection of a string" in {
    val attributeList = AttributeList(Attribute(GREEN, 5, 11))
    val subString = AttributedString("some string is stringy", attributeList)
    subString.toString shouldBe s"some ${GREEN}string$RESET is stringy"
  }

  it should "apply to several attributes" in {
    val string = AttributedString("this is green and this is red") < Attribute(GREEN, 8,13) < Attribute(RED, 26, 29)
    string.toString shouldBe s"this is ${GREEN}green$RESET and this is ${RED}red$RESET"
  }

  "+" should "concatenate plain strings" in {
    val str1 = AttributedString("hello") << RED
    val str2 = AttributedString(" there") << GREEN << MAGENTA_B
    val str3 = str1 + str2
    str3.length shouldBe 11
    str3.attributes.attributes shouldBe Seq(
        Attribute(RED, 0, 5),
        Attribute(Format(GREEN, MAGENTA_B), 5, 11)
    )
  }

  "substring" should "produce a substring of the string" in {
    unformattedString.subString(2,11).string shouldBe "formatted"
  }

  it should "cut off the beginning and end of any attribute out of the new range" in {
    val newStr = redString.subString(2,5)
    newStr.attributes.attributes shouldBe Seq(Attribute(RED, 0,3))
  }

  it should "maintain the correct range of the attributes" in {
    val testStr = AttributedString("foo foo foo", AttributeList(Attribute(RED_B, 4, 7)))
    testStr.subString(2, 9).attributes.attributes shouldBe Seq(Attribute(RED_B, 2,5))
  }

  "toUpperCase" should "make the string upper case" in {
    val result = redString.toUpperCase
    result shouldBe ("RED STRING" << RED)
  }

  "wrapToWidth" should "not modify the string if none of the lines are beyond the specified width" in {
    val as = AttributedString("this is a string") << RED
    as.wrapToWidth(100) shouldBe as
  }

  it should "wrap lines breaking at whitespace" in {
    val str = AttributedString("this is a very lovely string don't you think?")
    val expected =
      """this is a
        |very
        |lovely
        |string
        |don't you
        |think?""".stripMargin
    val broken = str.wrapToWidth(10)
    broken.string shouldBe AttributedString(expected).string
  }

  "trim" should "remove leading and training whitespace" in {
    AttributedString(" hello ").trim shouldBe AttributedString("hello")
  }

  "split" should "split by the regex" in {
    val str = AttributedString("one\ntwo\nthree")
    val split = str.split("\n")
    split shouldBe Seq(
      %("one"), %("two"), %("three")
    )
  }

  "padTo" should "pad a string to the desired length" in {
    val str = AttributedString("one")
    val res = str.padToLength(10, ' ').string shouldBe "one       "
  }

  it should "do nothing if the pad length is less than the string length" in {
    AttributedString("hello").padToLength(3, ' ').string shouldBe "hello"
  }

}
