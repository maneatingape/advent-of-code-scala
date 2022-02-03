package AdventOfCode2016

object Day03:
  def split(line: String): Seq[Int] = line.trim.split(" +").map(_.toInt).toSeq

  def triangle(row: Seq[Int]): Boolean =
    val Seq(a, b, c) = row
    a < b + c && b < a + c && c < a + b

  def part1(input: Seq[String]): Int = input.map(split).count(triangle)

  def part2(input: Seq[String]): Int = input.map(split).grouped(3).flatMap(_.transpose).count(triangle)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day03.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
