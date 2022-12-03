package AdventOfCode2022

object Day03:
  def priority(item: Char): Int = if (item.isUpper) item - 'A' + 27 else item - 'a' + 1

  def part1(input: Seq[String]): Int = input
    .map { line =>
      val (first, second) = line.splitAt(line.length / 2)
      val item = first.intersect(second)
      priority(item.head)
    }
    .sum

  def part2(input: Seq[String]): Int = input
    .grouped(3)
    .map { case Seq(first, second, third) =>
      val badge = first.intersect(second).intersect(third)
      priority(badge.head)
    }
    .sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day03.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
