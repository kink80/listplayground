package org.testlist

/**
 * A simple linked list with order based insertion. The whole structure is immutable, i.e. a fresh copy
 * gets created when an item gets added to/removed from it.
 *
 * @param value Initial head value
 * @param next Tail of the list
 */
case class ListNode[A: Ordering](
  value: Option[A],
  next: Option[ListNode[A]]
) {

  val o: Ordering[A] = implicitly[Ordering[A]]

  /**
   * Add an item to the list
   * @param a Item to add
   * @return A list containing newly added item in the sorted order
   */
  def +(a: A): ListNode[A] = {
    if (value.exists(o.lteq(a, _)) || value.isEmpty) {
      this.copy(value = Some(a), next = Some(this))
    } else {
      val tail = next match {
        case v@Some(_) => v.map(_ + a)
        case None => Some(ListNode[A](a))
      }
      this.copy(next = tail)
    }
  }

  /**
   * Remove a single item from the list
   * @param a Item to remove
   * @return A list without the item, note that once the list becomes empty a [[None]] gets returned
   */
  def -(a: A): ListNode[A] = {
    if (value.exists(o.equiv(_, a))) {
      next match {
        case Some(tail) => tail
        case None => ListNode(None, None)
      }
    } else {
      this.copy(next = next.map(_ - a))
    }
  }

  /**
   * Size of the list
   * @return Number of items in this list
   */
  def size: Int = value.map(_ => 1).getOrElse(0) + next.map(_.size).getOrElse(0)

  /**
   * Converts the linked list to a Scala compatible list
   * @return Instance of a [[List[A]]]
   */
  def toList: List[A] = {
    value match {
      case Some(v) => List(v) ++ next.map(_.toList).getOrElse(List.empty[A])
      case None => List.empty[A]
    }
  }

  override def toString: String = {
    toList.toString()
  }

}

object ListNode {

  /**
   * Companion enabling instantiation of single valued lists
   * @param value Initial value
   * @return A fresh instance of a linked list
   */
  def apply[A: Ordering](
    value: A
  ): ListNode[A] = {
    ListNode[A](Some(value), None)
  }

}
