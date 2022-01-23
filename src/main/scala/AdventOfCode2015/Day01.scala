package AdventOfCode2015

object Day01:
  def part1(input: String): Int = input.count(_ == '(') - input.count(_ == ')')

  def part2(input: String): Int =
    input.scanLeft(0)((total, next) => if next == '(' then total + 1 else total - 1).indexOf(-1)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day01.txt").mkString.trim
    println(part1(data))
    println(part2(data))
