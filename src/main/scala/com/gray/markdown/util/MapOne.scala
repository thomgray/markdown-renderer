package com.gray.markdown.util

trait MapOne {
  import scala.language.implicitConversions
  implicit def traversableToWrapper[T](traversable: TraversableOnce[T]): TraversableWrapper[T] = new TraversableWrapper[T](traversable)
  implicit def toPipe[T](t: T): PipeWrapper[T] = new PipeWrapper(t)

  class TraversableWrapper[T](traversable: TraversableOnce[T]) {
    def mapOne[U](f: (T) => Option[U]): Option[U] = {
      traversable.find(thing => f(thing) match {
        case Some(u) => return Some(u)
        case None => false
      })
      None
    }
  }

  class PipeWrapper[T](t: T) {
    def |[U] (f: (T) => U) = f(t)
  }

  def doMap[T,U](t: T)(f: (T) => Option[(T,U)]) = {
    def recMap(recT: T, uSoFar: Seq[U]): (T, Seq[U])  = {
      f(recT) match {
        case Some((innerT, innerU)) =>
          recMap(innerT, uSoFar :+ innerU)
        case None => recT -> uSoFar
      }
    }
    recMap(t, Seq.empty[U])
  }

}

