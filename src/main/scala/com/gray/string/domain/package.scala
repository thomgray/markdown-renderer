package com.gray.string

package object domain {


  type Range = (Int, Int)

  def intersection(range1: Range, rang2: Range): Option[Range] = {
    val highestLowerBound = Math.max(range1._1, rang2._1)
    val lowestUpperBound = Math.min(range1._2, rang2._2)
    if (highestLowerBound < lowestUpperBound) Some(highestLowerBound, lowestUpperBound) else None
  }

}
