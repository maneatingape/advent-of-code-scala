package AdventOfCode2022

object Day06:
  def find(input: String, size: Int): Int = input.sliding(size).indexWhere(_.toSet.size == size) + size

  def part1(input: String): Int = find(input, 4)

  def part2(input: String): Int = find(input, 14)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day06.txt").mkString
    println(part1(data))
    println(part2(data))
