package AdventOfCode2017

object Day02:
  def parse(input: Seq[String]): Seq[Seq[Int]] = input.map(_.split("\\D+").map(_.toInt).toSeq)

  def part1(input: Seq[String]): Int = parse(input).map(row => row.max - row.min).sum

  def part2(input: Seq[String]): Int = parse(input).flatMap { row =>
    row.combinations(2).map { case Seq(a, b) => if a.max(b) % a.min(b) == 0 then a.max(b) / a.min(b) else 0 }
  }
  .sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day02.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
