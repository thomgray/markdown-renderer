package com.gray.string.domain

import scala.io.AnsiColor

case class Format(foreground: Option[String] = None,
                  background: Option[String] = None,
                  other: Option[Seq[String]] = None
                 ) {

  def +(format: Format) = {
    val newForeground = (foreground, format.foreground) match {
      case (_, Some(f)) => Some(f)
      case (Some(f), _) => Some(f)
      case _ => None
    }
    val newBackground = (background, format.background) match {
      case (_, Some(f)) => Some(f)
      case (Some(f), _) => Some(f)
      case _ => None
    }
    val newOthers = (other, format.other) match {
      case (Some(f1), Some(f2)) => Some((f1 ++ f2) distinct)
      case (Some(f), _) => Some(f)
      case (_, Some(f)) => Some(f)
      case _ => None
    }
    Format(newForeground, newBackground, newOthers)
  }

  override def toString: String = {
    val fgc = stringForFormat(foreground.getOrElse(""))
    val bgc = stringForFormat(background.getOrElse(""))
    val oth = other.getOrElse(Nil).map(stringForFormat).mkString(",")
    s"$fgc,$bgc,$oth"
  }


  private def stringForFormat(string: String) = string match {
    case "" => ""
    case AnsiColor.BLACK => "BLACK"
    case AnsiColor.RED => "RED"
    case AnsiColor.GREEN => "GREEN"
    case AnsiColor.YELLOW => "YELLOW"
    case AnsiColor.BLUE => "BLUE"
    case AnsiColor.MAGENTA => "MAGENTA"
    case AnsiColor.CYAN => "CYAN"
    case AnsiColor.WHITE => "WHITE"
    case AnsiColor.BLACK_B => "BLACK_B"
    case AnsiColor.RED_B => "RED_B"
    case AnsiColor.GREEN_B => "GREEN_B"
    case AnsiColor.YELLOW_B => "YELLOW_B"
    case AnsiColor.BLUE_B => "BLUE_B"
    case AnsiColor.MAGENTA_B => "MAGENTA_B"
    case AnsiColor.CYAN_B => "CYAN_B"
    case AnsiColor.WHITE_B => "WHITE_B"
    case AnsiColor.RESET => "RESET"
    case AnsiColor.BOLD => "BOLD"
    case AnsiColor.UNDERLINED => "UNDERLINED"
    case AnsiColor.BLINK => "BLINK"
    case AnsiColor.REVERSED => "REVERSED"
    case AnsiColor.INVISIBLE => "INVISIBLE"
  }

  /**
    * returns the actual string escape sequence for this format
    * @return
    */
  def * = foreground.getOrElse("") + background.getOrElse("") + other.getOrElse(Nil).mkString
}