package cse250.pa2
/**
 * cse250.pa2.SortedListTests
 *
 * Copyright 2022 Oliver Kennedy (okennedy@buffalo.edu)
 *           2022 Eric Mikida (epmikida@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class SortedListTests extends AnyFlatSpec {

  /**
   * A "seeded" random instance that ensures that tests are 
   * deterministic.  The numbers that come out will be pseudorandom;
   * they'll be "random-like", but it'll be the same random sequence
   * each time, unless you change the seed value (The integer 250 
   * in our case).
   */
  val SeededRandom = new Random(250)


  behavior of "SortedList"
  it should "insert reverse-order elements in order" in 
  {
    val list = new SortedList[Int]()  

    // Inserting in reverse order should always insert at the head

    for(i <- Seq.range(start = 9, end = -1, step = -1) )
    { 
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range (-1, 9] == [0, 9] == [0, 10), every element should
    // be located at its index. 

    for((elem, index) <- list.zipWithIndex)
    {
      assert(elem == index)
    }
  }

  it should "insert in-order elements in order" in 
  {
    val list = new SortedList[Int]()  

    // Inserting in order should always insert at the tail

    for(i <- 0 until 10)
    { 
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range [0, 10), every element should be located at its index.

    for((elem, index) <- list.zipWithIndex)
    {
      assert(elem == index)
    }
  }

  it should "insert random-order elements in order" in 
  {
    val list = new SortedList[Int]()  

    // Inserting in random order should test all of the other cases of insert

    for(i <- SeededRandom.shuffle(IndexedSeq.range(start = 0, end = 30)) )
    { 
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range [0, 30), every element should be located at its index.

    for((elem, index) <- list.zipWithIndex)
    {
      assert(elem == index)
    }

    // Pick 10 distinct random indexes in the range [0, 30) and test
    // the apply method by checking to see if the element at that index
    // is equal to the index (following the same logic as the last test).

    for(index <- SeededRandom.shuffle(IndexedSeq.range(start = 0, end = 30)).take(10))
    {
      assert(list(index) == index)
    }
  }

  it should "insert in-order elements with hints" in
  {
    val list = new SortedList[Int]()  

    // The hinted version of the for loop should be quite a bit faster
    // for in-order insertions if we pass in a "hint" pointing at the
    // tail of the list (the last element we inserted).  If this test case
    // is a bit on the slow side, then you may have an O(n) implementation
    // of the hinted insert.

    var tail = list.insert(0)
    for(i <- 1 until 100)
    { 
      tail = list.insert(i, tail)
    }

    // Test using the same logic as above

    for((elem, index) <- list.zipWithIndex)
    {
      assert(elem == index)
    }
  }

  it should "efficiently update elements" in
  {
    val list = new SortedList[Int]()  

    // The hinted version of the for loop should be quite a bit faster
    // for in-order insertions if we pass in a "hint" pointing at the
    // tail of the list (the last element we inserted).  If this test case
    // is a bit on the slow side, then you may have an O(n) implementation
    // of the hinted insert.

    var first = list.insert(0)
    var tail = first
    for(i <- 1 until 2000)
    { 
      tail = list.insert(i*2, tail)
    }

    // Start with the head element of the list and move it forward 1000 spaces
    // If test case is a bit on the slow side, then you may have an O(n) 
    // implementation of either the hinted remove() or the hinted insert.

    for(i <- 0 until 1000){
      first = list.update(first, i * 2 + 1)
    }

    // Check to see if all of the elements are present and in-order
    for((elem, index) <- list.zipWithIndex)
    {
      if(index < 999){
        assert(elem == ((index+1)*2))
      } else if(index == 999) {
        assert(elem == 999*2+1)
      } else {
        assert(elem == index*2)
      }
    }
  }

  it should "be a linked list" in
  {
    val list = new SortedList[Int]()  

    var tail = list.insert(0)
    for(i <- 1 until 10)
    { 
      list.insert(i, tail)
    }

    var current = list.headNode
    assert(current.isDefined)
    for(i <- 0 until 10)
    {
      assert(current.isDefined)
      assert(current.get.value == i)
      current = current.get.next
    }
  }

  /*my impementation */
  //Define the search functions

  it should "def findRefBefore(elem: T): Option[SortedListNode[T]]"in{
      val list = new SortedList[Int]()
      list.insert(1)
      list.insert(3)
      list.insert(5)

      assert(list.findRefBefore(4).isDefined)
      assert(list.findRefBefore(4).get.value == 3)
      assert(list.findRefBefore(0).isEmpty)
      assert(list.findRefBefore(6).isDefined)
      assert(list.findRefBefore(6).get.value == 5)
  }
  it should "def findRefBefore(elem:T, hint: SortedListNode[T]): Option[SortedListNode[T]]"in{
    val list = new SortedList[Int]()
    list.insert(1)
    list.insert(3)
    list.insert(5)

    val result1 = list.findRefBefore(0)
    assert(result1 == None)


    val result2 = list.findRefBefore(-1)
    assert(result2 == None)


    val result3 = list.findRefBefore(3)
    assert(result3.isDefined)
    assert(result3.get.value == 1)

    val result4 = list.findRefBefore(7)
    assert(result4.isDefined)
    assert(result4.get.value == 5)


    val result5 = list.findRefBefore(4)
    assert(result5.isDefined)
    assert(result5.get.value == 3)


  }
  it should "def findRef(elem: T): Option[SortedListNode[T]]"in{
    val list = new SortedList[Int]()
    assert(list.findRef(5) == None)

    list.insert(3)
    list.insert(7)
    list.insert(11)
    list.insert(15)

    assert(list.findRef(3).get.value == 3)
    assert(list.findRef(7).get.value == 7)
    assert(list.findRef(11).get.value == 11)
    assert(list.findRef(15).get.value == 15)
    assert(list.findRef(8) == None)

  }
 it should "def findRef(elem: T, hint: SortedListNode[T]): Option[SortedListNode[T]]"in{
   val list = new SortedList[Int]()
   list.insert(1)
   list.insert(3)
   list.insert(5)
   list.insert(7)
   val hint = list.findRefBefore(6).get
   assert(list.findRef(3, hint) == Some(list.headNode.get.next))
   assert(list.findRef(6, hint) == None)
   assert(list.findRef(8, hint) == None)

 }
  //Define the indexing functions
  it should "def getRef(idx: Int): SortedListNode[T]" in {
    val list = new SortedList[Int]()
    list.insert(3)
    list.insert(1)
    list.insert(4)

    // Test valid index
    val node1 = list.getRef(0)
    assert(node1.value == 1)

    // Test valid index
    val node2 = list.getRef(1)
    assert(node2.value == 3)

    // Test valid index
    val node3 = list.getRef(2)
    assert(node3.value == 4)

    // Test invalid index
    try {
      val node4 = list.getRef(3)
      fail("Expected IndexOutOfBoundsException")
    } catch {
      case e: IndexOutOfBoundsException =>
    }
  }
it should "def apply(idx: Int): T"in{
  val list = new SortedList[Int]()
  list.insert(5)
  list.insert(2)
  list.insert(10)

  assert(list.apply(0) == 2)
  assert(list.apply(1) == 5)
  assert(list.apply(2) == 10)

  // test invalid index
  try {
    list.apply(-1)
    fail("Expected IndexOutOfBoundsException to be thrown")
  } catch {
    case _: IndexOutOfBoundsException => // expected
  }

  try {
    list.apply(3)
    fail("Expected IndexOutOfBoundsException to be thrown")
  } catch {
    case _: IndexOutOfBoundsException => // expected
  }

}
    //3. Define the insertion functions
it should "def insert(elem: T): SortedListNode[T]"in{
  val list = new SortedList[Int]()

  val node1 = list.insert(5)
  assert(node1.value == 5)

  val node2 = list.insert(10)
  assert(node2.value == 10)
  assert(node2.prev.get.value == 5)
  assert(node1.next.get.value == 10)

  val node3 = list.insert(8)
  assert(node3.value == 8)
  assert(node3.prev.get.value == 5)
  assert(node3.next.get.value == 10)
  assert(node2.prev.get.value == 8)
  assert(node1.next.get.value == 8)

  val node4 = list.insert(3)
  assert(node4.value == 3)
  assert(node4.next.get.value == 5)
  assert(list.headNode.get.value== 3)
  assert(node1.prev.isEmpty)

  val node5 = list.insert(8)
  assert(node5.value == 8)
  assert(node5.count == 2)
  assert(node5.prev.get.value == 5)
  assert(node5.next.get.value == 10)
  assert(node2.prev.get.value == 8)
  assert(node1.next.get.value == 8)

  val node6 = list.insert(15)
  assert(node6.value == 15)
  assert(node6.prev.get.value == 10)
  assert(node6.next.isEmpty)
  assert(node2.next.get.value == 15)

}
it should "def insert(elem: T, hint): SortedListNode[T]"in{
  val list = new SortedList[Int]()
  val node1 = list.insert(1)
  val node2 = list.insert(2)
  val node3 = list.insert(3)
  val node4 = list.insert(4)

  val newNode = list.insert(3, node1)
  assert(newNode.value == 3)

  val newNode2 = list.insert(0, node2)
  assert(newNode2.value == 0)

  val newNode3 = list.insert(5, node4)
  assert(newNode3.value == 5)

  assert(list.length == 7)
  assert(list.apply(0) == 0)
  assert(list.apply(1) == 1)
  assert(list.apply(2) == 2)
  assert(list.apply(3) == 3)
  assert(list.apply(4) == 3)
  assert(list.apply(5) == 4)
  assert(list.apply(6) == 5)

}


  //Define the removal functions

  it should "def remove(ref: SortedListNode[T]):" in {
    val list = new SortedList[Int]()
    list.insert(1)
    list.insert(2)
    list.insert(3)
    val node =list.headNode.get.next.get
    val value =list.remove(node)
    assert(value==2)
    assert(list.headNode.get.value==1)
    assert(list.headNode.get.next.get.value==3)

  }
  it should " removeN(ref: SortedListNode[T], n: Int): T"in {
    val list = new SortedList[Int]()
    list.insert(1)
    list.insert(2)
    list.insert(3)

    val node2 = list.headNode.get.next.get
    val removevalue2 = list.removeN(node2, 2)
    assert(removevalue2 == 2)
    assert(list.headNode.get.next.get.value == 3)

    val n = 4

    val node3 = list.headNode.get.next.get.next.get
    assertThrows[IllegalArgumentException] {
      list.removeN(node3, n)
    }

  }
  it should "def removeAll(ref: SortedListNode[T]): T"in{
    val list = new SortedList[Int]()
    val node1 = list.insert(1)
    val node2 = list.insert(2)
    val node3 = list.insert(2)
    val node4 = list.insert(3)
    val node5 = list.insert(4)

    node1.next = Some(node2)
    node2.next = Some(node3)
    node3.next = Some(node4)
    node4.next = Some(node5)

    var removedValue = list.removeAll(node4)
    assert(removedValue == 3)
    assert(node1.next.get.next.get.next.get.value == 4)

    removedValue = list.removeAll(node2)
    assert(removedValue == 2)
    assert(node1.value == 1)
    assert(node1.next.get.value == 3)
    assert(node1.next.get.next.get.value == 4)

    removedValue = list.removeAll(node1)
    assert(removedValue == 1)
    assert(node1.value == 2)
    assert(node1.next.get == 2)
    assert(node1.next.get.next.get.value == 3)
    assert(node1.next.get.next.get.next.get.value == 4)
  }

}
