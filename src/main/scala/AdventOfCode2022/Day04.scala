package AdventOfCode2022

object Day04:
  def parse(line: String): (Int, Int, Int, Int) =
    val Array(a, b, c, d) = line.split("\\D").map(_.toInt)
    (a, b, c, d)

  def part1(input: Seq[String]): Int = input.map(parse)
    .count((a, b, c, d) => (a >= c && b <= d) || (c >= a && d <= b))

  def part2(input: Seq[String]): Int = input.map(parse)
    .count((a, b, c, d) => a <= d && c <= b)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day04.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
