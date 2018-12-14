

package day1

import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {

    val finalFrequency = Source.fromResource("day1/input.txt").getLines().map(Integer.parseInt).sum

    val frequencies = Stream.continually(Source.fromResource("day1/input.txt").getLines().map(Integer.parseInt)).flatten.scanLeft(0)(_ + _)

    def firstDup(stream: Stream[Int], seen: Set[Int] = Set.empty[Int]): Int = {
      stream match {
        case head #:: tail if seen(head) => head
        case head #:: tail => firstDup(tail, seen + head)
      }
    }

    println(s"final frequency: $finalFrequency")
    println(s"first duplicate: ${firstDup(frequencies)}")

  }

}


