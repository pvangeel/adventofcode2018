package day2

import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {


    val checksum = Source.fromResource("day2/input.txt")
      .getLines()
      .flatMap(_.groupBy(identity).mapValues(_.length).values.toSet - 1)
      .toList
      .groupBy(identity)
      .mapValues(_.size)
      .values
      .product

    println(s"Checksum: $checksum")

    def numberOfDifferentCharacters(s1: String, s2: String) = s1 zip s2 count { case (c1, c2) => c1 != c2 }

    val lines = Source.fromResource("day2/input.txt").getLines().toList

    val part2 = (lines.map { line => (line, lines.groupBy(numberOfDifferentCharacters(line, _)) - 0) }
      .filter { case (a: String, b: Map[Int, List[String]]) => b.contains(1) }
      .map { case (a, b) => (a, b(1).head) }.head match {
      case (s1, s2) => s1.zip(s2).filter { case (c1, c2) => c1 == c2 }
    }).map {
      _._1
    }.mkString("")

    println(s"Part 2 solution: $part2")
  }


}
