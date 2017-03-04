package com.gray.markdown



import com.gray.markdown.produce.MdParser._
import com.gray.markdown.render.MdRenderer

object Main extends App with MdRenderer {
  val result = parse(args.mkString(" "))
  val rendered = render(result, 80)
  println(rendered)
}
