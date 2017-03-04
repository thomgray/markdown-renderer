package com.gray.string

import com.gray.string.domain.{Attribute, AttributeInterpreter, AttributeList, Format}

import scala.io.AnsiColor
import scala.util.matching.Regex

class AttributedString(val string: String, val attributes: AttributeList) extends AnsiColor {

  def length = string.length

  override def toString: String = {
    var out = ""
    var marker = 0
    for (attribute <- attributes.attributes) {
      if (marker < attribute.start) {
        out += string.substring(marker, attribute.start)
      }
      out += (attribute.format *)
      out += string.substring(attribute.start, attribute.end)
      out += RESET
      marker = attribute.end
    }
    if (marker < string.length) {
      out += string.substring(marker)
    }
    out
  }

  def <(attribute: Attribute) = {
    val newList = attributes + attribute
    AttributedString(string, newList)
  }

  def <<(format: Format) = {
    this < Attribute(format, 0, length)
  }

  def +(attributedString: AttributedString) = {
    val newAttributes = attributedString.attributes.attributes map (_.shift(length))
    val allAttributes = AttributeList.stitchContinuous(attributes.attributes ++ newAttributes)
    val newString = string + attributedString.string
    new AttributedString(newString, new AttributeList(allAttributes))
  }

  def subString(start: Int = 0, end: Int = length) = {
    val newLength = end - start
    val newAttributes = attributes.attributes.map(_.shift(-start))
      .filter(at => at.end > 0 && at.start < newLength)
      .map({
        case Attribute(format, start, end) =>
          val newStart = if (start < 0) 0 else start
          val newEnd = if (end > newLength) newLength else end
          Attribute(format, newStart, newEnd)
      })
    val newString = string.substring(start, end)
    AttributedString(newString, new AttributeList(newAttributes))
  }

  def toUpperCase() = new AttributedString(string.toUpperCase, attributes)

  def wrapToWidth(width: Int, wrapPrefix: Option[AttributedString] = None, wrapSuffix: Option[AttributedString] = None) = {
    val regex = wrapRegex(width)
    def wrapRecursive(toWrap: AttributedString, wrapTo: Option[AttributedString]): AttributedString = {
      val lengthOfChunk = regex.findFirstIn(toWrap.string) match {
        case Some(chunk) => chunk.length
        case _ => Math.min(width, toWrap.string.length)
      }
      val newString = wrapTo match {
        case Some(toString) =>
          toString.trim +
          AttributedString("\n") +
          wrapPrefix.getOrElse(AttributedString("")) +
          toWrap.subString(end = lengthOfChunk).trim
        case None => toWrap.subString(end=lengthOfChunk)
      }
      val suffix = toWrap.subString(lengthOfChunk)
      if (suffix.length > 0) wrapRecursive(suffix, Some(newString))
      else newString
    }
    wrapRecursive(this, None)
  }

  private def wrapRegex(width: Int) = s"""^.{0,${width - 1}}(\\s|_|-|$$)""".r

  override def equals(obj: scala.Any): Boolean = obj match {
    case AttributedString(otherString, otherAttributes) => string.equals(otherString) && attributes.equals(otherAttributes)
    case _ => false
  }

  def trim = {
    val prefix = "^\\s*".r.findFirstIn(string).getOrElse("").length
    val suffix = "\\s*$".r.findFirstIn(string).getOrElse("").length
    subString(prefix, length-suffix)
  }

  def split(regex: String) = {
    val regexProper = regex.r
    def recSplit(remainder: AttributedString, soFar: Seq[AttributedString]): Seq[AttributedString] = {
      regexProper.findFirstMatchIn(remainder.string) match {
        case Some(mtch) =>
          val s1 = remainder.subString(end = mtch.start)
          val remainder2 = remainder.subString(mtch.end)
          recSplit(remainder2, soFar :+ s1)
        case None =>
          soFar :+ remainder
      }
    }
    recSplit(this, Nil)
  }

}

object AttributedString {
  def apply(string: String): AttributedString = new AttributedString(string, AttributeList.empty)

  def apply(string: String, attributes: AttributeList): AttributedString = new AttributedString(string, attributes)

  def apply(string: String, attributes: String*) = {
    val attributeList = attributes match {
      case Nil => AttributeList.empty
      case other =>
        val format = AttributeInterpreter.formatFromStrings(other)
        AttributeList(Attribute(format, 0, string.length))
    }
    new AttributedString(string, attributeList)
  }

  def unapply(arg: AttributedString): Option[(String, AttributeList)] = Some(arg.string, arg.attributes)

}