package org.testlist

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListNodeSpec extends AnyFlatSpec with Matchers {

  "ListNode" should "be able to consume same values multiple times" in {
    (ListNode(1) + 1 + 1 + 1 + 1 + 1).toList should be (List(1, 1, 1, 1, 1, 1))
  }

  it should "apply natural ordering of int elements" in {
    (ListNode(234) + 1 + (-33) + 16 + 3 + 2).toList should be (List(-33, 1, 2, 3, 16, 234))
  }

  it should "apply natural ordering of string elements" in {
    // Default string ordering does not deal with i18n
    (ListNode("bar") + "baz" + "bad" + "foo").toList should be (List("bad", "bar", "baz", "foo"))
  }

  it should "be able to remove elements from the list" in {
    (ListNode(1) + 2 + 3 + 4 - 2 - 3).toList should be (List(1, 4))
  }

  it should "correctly count number of elements" in {
    (ListNode(1) + 2 + 3 + 4 - 2 - 3).size should be (2)
    (ListNode(1) + 1 + 1 + 1 + 1 + 1 - 1 - 1).size should be (4)
  }

  it should "not remove non-existent element from the list" in {
    (ListNode(1) + 2 + 3 - 5 + 4).toList should be (List(1, 2, 3, 4))
  }

  it should "produce an empty list once all elements had been removed" in {
    (ListNode(1) + 2 + 3 - 2 - 3 - 1).toList should be (List.empty)
  }

  it should "be able to add items to a previously emptied list" in {
    (ListNode(1) - 1 + 2 + 3).toList should be (List(2, 3))
  }

}
