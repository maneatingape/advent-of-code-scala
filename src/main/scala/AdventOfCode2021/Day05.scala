package AdventOfCode2021

object Day05:
  case class Line(x1: Int, y1: Int, x2: Int, y2: Int):
    def compare(a: Int, b: Int): Int = if a == b then 0 else if a < b then 1 else -1

    def isOrthogonal: Boolean = x1 == x2 || y1 == y2

    def points: Seq[(Int, Int)] =
      val stepX = compare(x1, x2)
      val stepY = compare(y1, y2)
      val length = Math.max(Math.abs(x1 - x2), Math.abs(y1 - y2))
      (0 to length).map(i => (x1 + stepX * i, y1 + stepY * i))

  def parseLines(input: Seq[String]): Seq[Line] = input
    .map(_.split(" -> ").flatMap(_.split(",")).map(_.toInt))
    .map { case Array(x1, y1, x2, y2) => Line(x1, y1, x2, y2) }

  def overlap(lines: Seq[Line]): Int = lines.flatMap(_.points).groupBy(identity).count(_._2.length > 1)

  def part1(input: Seq[String]): Int = overlap(parseLines(input).filter(_.isOrthogonal))

  def part2(input: Seq[String]): Int = overlap(parseLines(input))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day05.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
