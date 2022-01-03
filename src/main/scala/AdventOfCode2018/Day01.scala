package AdventOfCode2018

import scala.annotation.tailrec

object Day01:
  def part1(input: Seq[Int]): Int = input.sum

  def part2(input: Seq[Int]): Int =
    def helper(index: Int, frequency: Int, previous: Set[Int]): Int =
      val nextFrequency = frequency + input(index % input.size)
      if previous.contains(nextFrequency) then nextFrequency
      else helper(index + 1, nextFrequency, previous + nextFrequency)

    helper(0, 0, Set())
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day01.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
