package com.gray.string.domain

import org.scalatest.{FlatSpec, Matchers}
import com.gray.util.ImplicitConversions

import scala.io.AnsiColor

class AttributeListSpec extends FlatSpec with Matchers with AnsiColor with ImplicitConversions {
  val a1 = Attribute(RED, 0,10)
  val a2 = Attribute(BLUE_B, 0,10)

  val a3 = Attribute(stringToFormat(YELLOW), 15,20)
  val a4 = Attribute(CYAN_B, 5,15)

  "+" should "merge attributes of the same range" in {
    val list = new AttributeList(Seq(a1)) + a2
    list.attributes.length shouldBe 1
    list.attributes.head shouldBe Attribute(Format(RED, BLUE_B), 0,10)
  }

  it should "add non conflicting atttributes" in {
    val list = AttributeList(a1) + a3
    list.attributes shouldBe Seq(a1, a3)
  }

  it should "merge overlapping attributes" in {
    val list = AttributeList(a1) + a4
    val expected1 = Attribute(RED, 0,5)
    val expected2 = Attribute(Format(RED, CYAN_B), 5,10)
    val expected3 = Attribute(CYAN_B, 10,15)
    list.attributes shouldBe Seq(expected1, expected2,expected3)
  }

  it should "merge attributes when a new attribute overwrites a segment of an existing" in {
    val a5 = Attribute(BLACK_B, 5,7)
    val list = AttributeList(a1) + a5

    list.attributes shouldBe Seq(
      Attribute(RED, 0,5),
      Attribute(Format(RED, BLACK_B), 5,7),
      Attribute(RED, 7,10)
    )
  }

  it should "merge attributes when a new attribute overlaps several existing" in {
    val a5 = Attribute(GREEN, 10, 20)
    val list = new AttributeList(Seq(a1, a5))
    (list + a4).attributes shouldBe Seq(
      Attribute(RED, 0,5),
      Attribute(Format(RED, CYAN_B), 5,10),
      Attribute(Format(GREEN, CYAN_B), 10,15),
      Attribute(GREEN, 15,20)
    )
  }

  it should "concatenate identical attributes that touch or overlap" in {
    val a1 = Attribute(RED, 0, 6)
    val a2 = Attribute(RED, 6, 10)
    val list = AttributeList(a1) + a2
    list.attributes shouldBe Seq(Attribute(RED, 0, 10))
  }

  "mergePart" should "add an attribute if it doesn't interfere with any existing" in {
    val list = AttributeList(a1)
    list.mergePart(a3) shouldBe Seq(a1, a3)
  }

  it should "merge overlapping attributes" in {
    val list = AttributeList(a1)
    val expected1 = Attribute(RED, 0,5)
    val expected2 = Attribute(Format(RED, CYAN_B), 5,10)
    val expected3 = Attribute(CYAN_B, 10,15)

    list.mergePart(a4) shouldBe Seq(expected1, expected2, expected3)
  }

  "stitchContinuous" should "do nothing to a non-continuous list" in {
    val a1 = Attribute(RED, 0,3)
    val a2 = Attribute(GREEN, 5,7)
    val seq = Seq(a1, a2)
    val list = AttributeList(Nil)

    list.stitchContinuous(seq) shouldBe seq
  }

  it should "merge adjacent attributes of the same format" in {
    val a1 = Attribute(RED, 0,5)
    val a2 = Attribute(RED, 5, 10)
    val seq = Seq(a1, a2)
    val list = AttributeList(Nil)

    list.stitchContinuous(seq) shouldBe Seq(Attribute(RED, 0, 10))
  }


}
