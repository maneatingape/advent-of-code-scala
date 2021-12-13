package AdventOfCode2020

object Day06:
  val whitespace = Set(' ', '\n')

  def parse(input: String)(answers: String => Set[Char]): Int = input.trim.split("\n\n").map(answers).map(_.size).sum

  def part1(input: String): Int = parse(input)(_.toSet -- whitespace)

  def part2(input: String): Int = parse(input) { line =>
    val sets = line.split("\n").map(_.toSet -- whitespace)
    sets.tail.foldLeft(sets.head)((total, next) => total.intersect(next))
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day06.txt").mkString
    println(part1(data))
    println(part2(data))
