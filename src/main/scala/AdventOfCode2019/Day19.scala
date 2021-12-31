package AdventOfCode2019

import Day09.IntCode

object Day19:
  def part1(memory: Seq[Long]): Long =
    val points = for x <- 0 until 50; y <- 0 until 50 yield IntCode(memory).withInput(x, y).allOutput
    points.flatten.sum

  def part2(memory: Seq[Long]): Long =
    def test(x: Long, y: Long) = IntCode(memory).withInput(x, y).allOutput.head == 1L

    def helper(x: Long, y: Long): Long =
      val top = test(x, y - 99)
      val left = test(x - 99, y)
      if top && left then 10000 * (x - 99) + (y - 99)
      else if top then helper(x + 1, y)
      else helper(x, y + 1)

    helper(99, 99)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day19.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))
    println(part2(data))
