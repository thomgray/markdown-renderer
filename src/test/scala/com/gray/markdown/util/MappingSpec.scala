package com.gray.markdown.util

import org.scalatest.{FlatSpec, Matchers}

class MappingSpec extends FlatSpec with MapOne with Matchers {

  "doMap" should "do map until the function orders a termination" in {
    val result = doMap(0){
      case 3 => None
      case other => Some(other+1 -> "BOO")
    }
    result shouldBe 3 -> Seq("BOO", "BOO", "BOO")
  }

}
