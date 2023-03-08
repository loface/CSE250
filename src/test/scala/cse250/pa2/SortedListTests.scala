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
  it should "set newNode.next and newNode.prev appropriately" in {
      val list = new SortedList[Int]()
      val node1 = list.insert(1)
      val node2 = list.insert(3, node1)
      val newNode = list.insert(2, node1)
      assert(newNode.prev == (node1))
      assert(newNode.next == (node2))
      assert(node1.next == (newNode))
      assert(node2.prev == (newNode))


  }
  it should "newNode.prev.next needs to be updated if it exists" in {
    val list = new SortedList[Int]()
    val node1 = list.insert(1)
    val node2 = list.insert(2, node1)
    val node3 = list.insert(3, node2)
    val newNode = list.insert(5, node1)
    assert(node1.next.get == newNode)
  }
  it should "newNode.next.prev needs to be updated if it exists" in {

      val list = new SortedList[Int]()
      val node1 = list.insert(1)
      val node2 = list.insert(5, node1)
      val node3 = list.insert(8,node2)
      val newNode = list.insert(2,node1)
      assert(node1.next.get.prev == newNode)
      assert(newNode.next.get.prev == newNode)
    }

  it should "update the headNode when inserting at the front" in {
      val list = new SortedList[Int]()

      // Inserting at the front should always update headNode

      for (i <- 9 to 0 by -1) {
        list.insert(i)
      }

      for ((elem, index) <- list.zipWithIndex) {
        assert(elem == index)
      }


  }
  it should "update lastNode when inserting at the end" in {
    val list = new SortedList[Int]()

    var tail = list.insert(0)
    for (i <- 1 until 7) {
      tail = list.insert(i, tail)
    }

    assert(list.lastNode.isDefined)
    assert(list.lastNode.get.value == 6)

    tail = list.insert(7, tail)


    assert(list.lastNode.isDefined)
    assert(list.lastNode.get.value == 7)
  }
  it should "update length and count when inserting a node with the same value" in {
    val list = new SortedList[Int]()
    list.insert(1)
    list.insert(2)
    list.insert(3)
    list.insert(2)
    list.insert(1)
    list.insert(2)
    list.insert(3)
    list.insert(3)
    list.insert(3)

    assert(list.length == 9)
    assert(list.headNode.get.value == 1)
    assert(list.lastNode.get.value == 3)


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
    list.insert(2)
    list.insert(3)

    val node2 = list.headNode.get.next.get
    val removevalue2 = list.removeN(node2, 2)
    assert(removevalue2 == 2)
    assert(list.headNode.get.next.get.value == 3)

    val n = 4

    val node3 = list.headNode.get.next.get.next.get
    assertThrows(IllegalArgumentException)
    list.removeN(node3, n)
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
    assert(node1.next.get.next.get.value == 4)

    removedValue = list.removeAll(node2)
    assert(removedValue == 2)
    assert(node1.next.get.value == 1)
    assert(node1.next.get.next.get.value == 3)
    assert(node1.next.get.next.get.next.get.value == 4)

    removedValue = list.removeAll(node1)
    assert(removedValue == 1)
    assert(node1.value == 3)
    assert(node1.next.get.value == 4)
  }
  it should "def removeAll(ref: SortedListNode[T]): T"in {
    val list = new SortedList[Int]()
    // Test case for remove
    val node1 = list.insert(1)
    val node2 = list.insert(2)
    val node3 = list.insert(3)
    val node4 = list.insert(3)
    val node5 = list.insert(4)
    val node6 = list.insert(5)
    node1.next = Some(node2)
    node2.next = Some(node3)
    node3.next = Some(node4)
    node4.next = Some(node5)
    node5.next = Some(node6)

    assert(list.remove(node4) == 3)
    assert(node3.next eq node5)
    assert(list.remove(node1) == 1)
    assert(node1.next eq node2)

    // Test case for removeN
    val node7 = list.insert(2)
    val node8 = list.insert(2)
    val node9 =list.insert(3)
    val node10 = list.insert(4)
    val node11 = list.insert(4)
    node7.next = Some(node8)
    node8.next = Some(node9)
    node9.next = Some(node10)
    node10.next = Some(node11)

    assert(list.removeN(node8, 2) == 2)
    assert(node7.next eq node9)
    assert(list.removeN(node10, 1) == 4)
    assert(node10.next eq node11)
    assert(list.removeN(node9, 0) == 3)
    try {
      list.removeN(node11, 3)
      assert(false, "IllegalArgumentException not raised")
    } catch {
      case _: IllegalArgumentException => assert(true)
      case _ => assert(false, "Unexpected exception")
    }

    // Test case for removeAll
    val node12 = list.insert(1)
    val node13 = list.insert(2)
    val node14 = list.insert(2)


  }

}
