package AdventOfCode2016

object Day20:
  case class Range(start: Long, end: Long):
    def size = end - start + 1
    def subtract(other: Range): Seq[Range] =
      val (s1, e1) = (start, end)
      val (s2, e2) = (other.start, other.end)
      if s2 <= s1 && e1 <= e2 then Seq()
      else if s1 < s2 && e2 < e1 then Seq(Range(s1, s2 - 1), Range(e2 + 1, e1))
      else if s1 <= e2 && e2 < e1 then Seq(Range(e2 + 1, e1))
      else if s1 < s2 && s2 <= e1 then Seq(Range(s1, s2 - 1))
      else Seq(this)

  def parse(input: Seq[String]): Seq[Range] = input.map { line =>
    val Array(start, end) = line.split("-").map(_.toLong)
    Range(start, end)
  }

  def intersect(input: Seq[String], max: Long): Seq[Range] =
    val initial = Seq(Range(0, max))
    parse(input).foldLeft(initial)((remaining, next) => remaining.flatMap(_.subtract(next)))

  def part1(input: Seq[String], max: Long): Long = intersect(input, max).map(_.start).min

  def part2(input: Seq[String], max: Long): Long = intersect(input, max).map(_.size).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day20.txt").getLines().toSeq
    val max = 4294967295L
    println(part1(data, max))
    println(part2(data, max))
