package AdventOfCode2017

import scala.annotation.tailrec

object Day06:
  def parse(input: String): Seq[Int] = input.split("\\D+").map(_.toInt).toSeq

  def step(banks: Seq[Int]): Seq[Int] =
    val max = banks.max
    val start = banks.indexWhere(_ == max)
    (1 to max).foldLeft(banks.updated(start, 0)) { (banks, offset) =>
      val index = (start + offset) % banks.size
      banks.updated(index, banks(index) + 1)
    }

  def findCycle(input: String): (Int, Int) =
    @tailrec
    def helper(current: Seq[Int], previous: Map[Seq[Int], Int], depth: Int): (Int, Int) =
      if previous.contains(current) then (depth, depth - previous(current))
      else helper(step(current), previous.updated(current, depth), depth + 1)

    helper(parse(input), Map(), 0)
  end findCycle

  def part1(input: String): Int = findCycle(input)._1

  def part2(input: String): Int = findCycle(input)._2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day06.txt").mkString.trim
    println(part1(data))
    println(part2(data))
