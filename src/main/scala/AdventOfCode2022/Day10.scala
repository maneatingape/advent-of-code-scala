package AdventOfCode2022

object Day10:
  def parse(input: String): Array[Int] =
    input.split("\\s+").map(_.toIntOption.getOrElse(0)).scanLeft(1)(_ + _)

  def part1(input: String): Int =
    val signal = parse(input)
    (20 to 220 by 40).map(cycle => cycle * signal(cycle - 1)).sum

  def part2(input: String): String = parse(input)
    .init.grouped(40)
    .map(_.zipWithIndex.map((x, col) => if (x - col).abs <= 1 then '#' else '.'))
    .map(_.mkString).mkString("\n")

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day10.txt").mkString
    println(part1(data))
    println(part2(data))
