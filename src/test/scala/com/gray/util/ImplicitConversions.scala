package com.gray.util

import com.gray.markdown.{MdLocation, MdString}
import com.gray.string.AttributedString
import com.gray.string.domain.Format
import sun.security.util.PendingException

import scala.language.implicitConversions

trait ImplicitConversions {
  import scala.io.AnsiColor._

  implicit def thingToOpt[T](thing: T): Some[T] = Some(thing)

  implicit def stringToFormat(string: String): Format = string match {
    case BLACK => Format(foreground = BLACK)
    case RED => Format(foreground = RED)
    case GREEN => Format(foreground = GREEN)
    case YELLOW => Format(foreground = YELLOW)
    case BLUE => Format(foreground = BLUE)
    case MAGENTA => Format(foreground = MAGENTA)
    case CYAN => Format(foreground = CYAN)
    case WHITE => Format(foreground = WHITE)


    case BLACK_B => Format(background = BLACK_B)
    case RED_B => Format(background = RED_B)
    case GREEN_B => Format(background = GREEN_B)
    case YELLOW_B => Format(background = YELLOW_B)
    case BLUE_B => Format(background = BLUE_B)
    case MAGENTA_B => Format(background = MAGENTA_B)
    case CYAN_B => Format(background = CYAN_B)
    case WHITE_B => Format(background = WHITE_B)

    case BOLD => Format(other = Some(List(BOLD)))
    case UNDERLINED => Format(other = Some(List(UNDERLINED)))

    case _ => throw new PendingException()
  }

  implicit def stringToAttributedString(string: String): AttributedString = AttributedString(string)

  implicit def toList[T](t:T): List[T] = List(t)
  implicit def stringToMdString(string: String): MdString = MdString(string, MdLocation(0,string.split("\n").length))
}
