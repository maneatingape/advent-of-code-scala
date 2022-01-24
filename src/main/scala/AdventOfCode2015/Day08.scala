package AdventOfCode2015

object Day08:
  def part1(input: Seq[String]): Int = input.map { line =>
    line.length - line.tail.init.replace("\\\\", "S").replace("\\\"", "Q").replaceAll("\\\\x..", "X").length
  }
  .sum

  def part2(input: Seq[String]): Int = input.map(line => 2 + line.count(c => c == '\\' || c == '\"')).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day08.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
