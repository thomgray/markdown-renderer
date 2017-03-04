package com.gray.string.domain

import org.scalatest.{FlatSpec, Matchers}

class domainSpec extends FlatSpec with Matchers{

  "intersection" should "return identical range if the ranges are identical" in {
    intersection((1,2), (1,2)) shouldBe Some((1,2))
  }

  it should "return the inner range" in {
    intersection((1,10), (5,6)) shouldBe Some((5,6))
  }

  it should "return a classic intersection" in {
    intersection((0,10), (5,20)) shouldBe Some((5, 10))
  }

  it should "return none if there is no intersection" in {
    intersection((0,3), (5,7)) shouldBe None
  }
}
