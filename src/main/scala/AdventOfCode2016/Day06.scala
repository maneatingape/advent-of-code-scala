package AdventOfCode2016

object Day06:
  def frequency(seq: Seq[Char]): Seq[Char] = seq.groupBy(identity).toSeq.sortBy(_._2.size).map(_._1)

  def part1(input: Seq[String]): String = input.transpose.map(frequency(_).last).mkString

  def part2(input: Seq[String]): String = input.transpose.map(frequency(_).head).mkString

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day06.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
