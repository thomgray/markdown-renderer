package com.gray.markdown.render.renderingrules

import com.gray.markdown.{MdLinkReference, MdParagraph, MdString, MdTable}
import com.gray.string
import com.gray.string.AttributedString
import com.gray.string.domain.Format

import scala.io.AnsiColor

object MdTableRenderer extends MdParagraphRenderer {
  override def render(mdParagraph: MdParagraph,
                      width: Int,
                      linkRefs: List[MdLinkReference],
                      renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
                     ): Option[AttributedString] = {
    mdParagraph match {
      case table: MdTable => Some(renderTable(table, width, linkRefs, renderer))
      case _ => None
    }
  }

  def renderTable(mdTable: MdTable,
                  width: Int,
                  linkRefs: List[MdLinkReference],
                  renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString
                 ): AttributedString = {

    val renderedRows = renderTableData(mdTable, width, linkRefs, renderer)
    drawTableBoxes(renderedRows).reduce(_+ newLine +_)
  }

  def renderTableData(table: MdTable, width: Int, linkRefs: List[MdLinkReference], renderer: (MdParagraph, Int, List[MdLinkReference]) => AttributedString) = {
    val headers = table.headers.map(renderer(_, -1, linkRefs))
    val data = table.data.map(_.data.map(renderer(_, -1, linkRefs)))

    val headers2 = headers.map(s => blankSpace + (s << BOLD) + blankSpace)
    val data2 = data.map(_.map(blankSpace + _ + blankSpace))


    val columns = switch(headers2 +: data2)
    val maxWidths = columns.map(_.foldLeft(0){ (i, l) => if (l.length > i) l.length else i})

    maxWidths.indices.toList.map( i => {
      columns(i).map(_.padToLength(maxWidths(i), ' '))
    }) | switch
  }

  def getColumns(table: MdTable) = {
    val allRows = table.headers +: table.data.map(_.data)
    switch(allRows)
  }

  protected[renderingrules] def drawTableBoxes(rows: List[List[AttributedString]]) = {
    val widths = rows.head.map(_.length)
    val topLine = rowLine('┌','─','┬','┐', widths)
    val linesRows = rows.map(vLine + _.reduce(_ + vLine + _)  + vLine)
    val headerSeparatorLine = rowLine('╞', '═', '╪', '╡', widths)
    val separators = rowLine('├', '─', '┼', '┤', widths)
    val bottomLine = rowLine('└','─','┴','┘', widths)
    val allSeparators = topLine +: headerSeparatorLine +: (0 until rows.length-2).map(_ => separators).toList
    allSeparators.zip(linesRows).flatMap(l => List(l._1, l._2)) :+ bottomLine
  }

  protected[renderingrules] def rowLine(left: Char, plain: Char, center: Char, right: Char, widths: List[Int]) = {
    val plainString = left +: widths.map("".padTo(_, plain)).mkString(center.toString) :+ right
    AttributedString(plainString)
  }

  private val vLine = AttributedString("│")
  private val blankSpace = AttributedString(" ")
  private val BOLD = Format(other = Some(List(AnsiColor.BOLD)))

  protected def switch[T](list: List[List[T]]) = {
    list.head.indices.toList.map(i => list.map(_(i)))
  }

}
