package AdventOfCode2017

import scala.annotation.tailrec

object Day05:
  def part1(input: Seq[Int]): Int =
    @tailrec
    def helper(jumps: Seq[Int], index: Int, steps: Int): Int =
      if index >= input.size then steps
      else helper(jumps.updated(index, jumps(index) + 1), index + jumps(index), steps + 1)

    helper(input, 0, 0)
  end part1

  def part2(input: Seq[Int]): Int =
    @tailrec
    def helper(jumps: Seq[Int], index: Int, steps: Int): Int =
      if index >= input.size then steps
      else if jumps(index) >= 3 then helper(jumps.updated(index, jumps(index) - 1), index + jumps(index), steps + 1)
      else helper(jumps.updated(index, jumps(index) + 1), index + jumps(index), steps + 1)

    helper(input, 0, 0)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day05.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
