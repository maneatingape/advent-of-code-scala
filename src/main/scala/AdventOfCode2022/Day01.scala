package AdventOfCode2022

object Day01:
  def parse(input: String): Seq[Int] =
    input.split("\n\n").map(_.split("\n").map(_.toInt).sum).toSeq

  def part1(input: String): Int = parse(input).max

  def part2(input: String): Int = parse(input).sorted.takeRight(3).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day01.txt").mkString
    println(part1(data))
    println(part2(data))
