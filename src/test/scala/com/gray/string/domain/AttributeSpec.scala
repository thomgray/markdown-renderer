package com.gray.string.domain

import org.scalatest.{FlatSpec, Matchers}
import com.gray.util.ImplicitConversions

import scala.language.implicitConversions

class AttributeSpec extends FlatSpec with Matchers with ImplicitConversions {


  val formatDud = Format()

  "shift" should "move the range up" in {
    val attribute = Attribute(formatDud, 0, 10)
    val attributeShifted = attribute.shift(3)

    attributeShifted.start shouldBe 3
    attributeShifted.end shouldBe 13

  }

  it should "move the range down" in {
    val attribute = Attribute(formatDud, 10, 20)
    val attributeShifted = attribute.shift(-3)

    attributeShifted.start shouldBe 7
    attributeShifted.end shouldBe 17
  }

  private val attribute = Attribute(formatDud, 10, 20)

  "splitOverRange" should "return nothing if the intersection is empty" in {
    attribute.splitOverRange(0,5) shouldBe (None, Nil)
  }

  it should "return the original attribute if within the range" in {
    attribute.splitOverRange(0, 100) shouldBe (Some(attribute), Nil)
  }

  it should "return the left side with the remainder if the range is bounded to the left" in {
    val expectedInRange = Attribute(formatDud, 10,15)
    val expectedOutsideRange = Attribute(formatDud, 15,20)

    attribute.splitOverRange(0, 15) shouldBe (Some(expectedInRange), Seq(expectedOutsideRange))
  }

  it should "return the right side with the remainder if the range is bounded to the right" in {
    val expectedInRange = Attribute(formatDud, 15,20)
    val expectedOutsideRange = Attribute(formatDud, 10,15)

    attribute.splitOverRange(15, 30) shouldBe (Some(expectedInRange), Seq(expectedOutsideRange))
  }

  it should "return the inner attribute with both left and right trims if the range is inside the attribute range" in {
    val expectedInRange = Attribute(formatDud, 12,17)
    val expectedLeft = Attribute(formatDud, 10,12)
    val expectedRight = Attribute(formatDud, 17, 20)

    attribute.splitOverRange(12,17) shouldBe (Some(expectedInRange), Seq(expectedLeft, expectedRight))
  }
}
