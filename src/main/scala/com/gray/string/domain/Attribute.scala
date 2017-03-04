package com.gray.string.domain

import scala.io.AnsiColor

class Attribute(val format: Format, val start: Int, val end: Int) {

  def shift(int: Int) = Attribute(format, start+int, end+int)

  def splitOverRange(x: Int, y: Int): (Option[Attribute], Seq[Attribute]) = {
    intersection((start, end), (x,y)) match {
      case None => (None, Nil)
      case Some(range) if range == (start, end) => (Some(this), Nil)
      case Some((i1, i2)) =>
        val inRangeAttribute = Attribute(format, i1, i2)
        val before = if (start < i1) Seq(Attribute(format, start, i1)) else Nil
        val after = if (end > i2) Seq(Attribute(format, i2, end)) else Nil
        (Some(inRangeAttribute), before ++ after)
    }
  }

  def mergeFormat(newFormat: Format) = {
    Attribute(format + newFormat, start, end)
  }

  override def toString: String = s"Attribute(\'$format, $start -> $end)"

  override def equals(obj: scala.Any): Boolean = obj match {
    case Attribute(otherFormat, otherstart, otherend) => format==otherFormat && start==otherstart && end==otherend
    case _ => false
  }

  def range = (start, end)

}

object Attribute {
  def apply(format: Format, start: Int, end: Int): Attribute = new Attribute(format, start, end)

  def unapply(arg: Attribute): Option[(Format, Int, Int)] = Some((arg.format, arg.start, arg.end))
}
