package com.gray.string.domain

import scala.io.AnsiColor

object AttributeInterpreter extends AnsiColor {

  def formatFromStrings(strings: Seq[String]) = {
    var base = stringToFormat(strings.head)
    val tail = strings.tail
    tail.foreach(s => base = base + stringToFormat(s))
    base
  }

  def stringToFormat(string: String): Format = string match {
    case BLACK => Format(foreground = Some(BLACK))
    case RED => Format(foreground = Some(RED))
    case GREEN => Format(foreground = Some(GREEN))
    case YELLOW => Format(foreground = Some(YELLOW))
    case BLUE => Format(foreground = Some(BLUE))
    case MAGENTA => Format(foreground = Some(MAGENTA))
    case CYAN => Format(foreground = Some(CYAN))
    case WHITE => Format(foreground = Some(WHITE))


    case BLACK_B => Format(background = Some(BLACK_B))
    case RED_B => Format(background = Some(RED_B))
    case GREEN_B => Format(background = Some(GREEN_B))
    case YELLOW_B => Format(background = Some(YELLOW_B))
    case BLUE_B => Format(background = Some(BLUE_B))
    case MAGENTA_B => Format(background = Some(MAGENTA_B))
    case CYAN_B => Format(background = Some(CYAN_B))
    case WHITE_B => Format(background = Some(WHITE_B))
  }

}
