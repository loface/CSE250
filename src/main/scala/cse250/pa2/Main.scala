package cse250.pa2
/**
 * cse250.pa2.Main
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

import scala.util.Random

object Main
{
  def main(args: Array[String])
  {
    // Think about how the details of our runtime affects the runtime of these loops
    // These loops should run very fast...if they do not, your implementation might
    // not match the specifications.
    println("Rolling a d20 1000000 times")
    val rolls = new SortedList[Int]
    for (i <- 0 until 1000000) {
      rolls.insert(Random.nextInt(20) + 1)
    }

    for (i <- 1 to 20) {
      var node = rolls.findRef(i)
      println("Rolled " + node.get.value.toString + " " + node.get.count.toString + " times")
    }
  }
}