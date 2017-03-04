package com.gray.string.domain

class AttributeList(val attributes: Seq[Attribute] = Nil) {

  def +(attribute: Attribute): AttributeList = {
    val newlist = stitchContinuous(mergePart(attribute))
    new AttributeList(newlist)
  }

  def shift(int: Int) = new AttributeList(attributes.map(_.shift(int)))

  private[domain] def mergePart(attribute: Attribute, currentList: Seq[Attribute] = attributes): Seq[Attribute] = {
    currentList.find(exitingAttribute => {
      exitingAttribute.splitOverRange(attribute.start, attribute.end) match {
        case (Some(existingInRange), remainders) =>
          val newExistingInRange = existingInRange.mergeFormat(attribute.format)
          var newCurrentList = currentList.filter(_ != exitingAttribute) ++ remainders :+ newExistingInRange
          val (_, remainingAttributes) = attribute.splitOverRange(existingInRange.start, existingInRange.end)
          for (at <- remainingAttributes) {
            newCurrentList = mergePart(at, newCurrentList)
          }
          return newCurrentList.sortWith(sorter)
        case (None, _) => false
      }
    })
    (currentList :+ attribute).sortWith(sorter)
  }

  def stitchContinuous(seq: Seq[Attribute]) = {
    seq.foldLeft(Seq.empty[Attribute]) { (list, attribute) =>
      list.lastOption match {
        case Some(last) if last.format == attribute.format && last.end == attribute.start =>
          list.slice(0, list.length-1) :+ Attribute(attribute.format, last.start, attribute.end)
        case _ => list :+ attribute
      }
    }
  }

  private def sorter(attribute1: Attribute, attribute2: Attribute): Boolean = attribute1.start <= attribute2.start

  override def equals(obj: scala.Any): Boolean = obj match {
    case AttributeList(otherAttributes) => attributes.equals(otherAttributes)
    case _ => false
  }

  override def toString: String = attributes.toString()

}

object AttributeList {
  def apply(attribute: Attribute): AttributeList = new AttributeList(Seq(attribute))

  def apply(attributes: Seq[Attribute]): AttributeList = {
    def list(seq: Seq[Attribute] = attributes, listSoFar: AttributeList = AttributeList.empty): AttributeList = seq match {
      case Nil => listSoFar
      case head :: tail => list(tail, listSoFar + head)
    }
    list()
  }

  def stitchContinuous(seq: Seq[Attribute]) = {
    seq.foldLeft(Seq.empty[Attribute]) { (list, attribute) =>
      list.lastOption match {
        case Some(last) if last.format == attribute.format && last.end == attribute.start =>
          list.slice(0, list.length-1) :+ Attribute(attribute.format, last.start, attribute.end)
        case _ => list :+ attribute
      }
    }
  }

  def unapply(arg: AttributeList): Option[Seq[Attribute]] = Some(arg.attributes)

  def empty = new AttributeList(Nil)
}
