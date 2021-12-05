package AdventOfCode2021

import java.lang.Math.{abs, max}

object Day05:
  case class Point(x: Int, y: Int)

  case class Line(p1: Point, p2: Point):
    def compare(a: Int, b: Int) = if a == b then 0 else if a < b then 1 else -1

    def isOrthogonal = p1.x == p2.x || p1.y == p2.y

    def points: Seq[Point] =
      val stepX = compare(p1.x, p2.x)
      val stepY = compare(p1.y, p2.y)
      val length = max(abs(p1.x - p2.x), abs(p1.y - p2.y))
      (0 to length).map(i => Point(p1.x + stepX * i, p1.y + stepY * i))

  def parseLines(input: Seq[String]): Seq[Line] = input
    .map(_.split(" -> ").flatMap(_.split(",")).map(_.toInt))
    .map { case Array(x1, y1, x2, y2) => Line(Point(x1, y1), Point(x2, y2)) }

  def overlap(lines: Seq[Line]): Int = lines
    .toSet
    .subsets(2)
    .map(_.toSeq)
    .flatMap { case Seq(line1, line2) => line1.points.intersect(line2.points) }
    .distinct
    .length

  def part1(input: Seq[String]): Int = overlap(parseLines(input).filter(_.isOrthogonal))

  def part2(input: Seq[String]): Int = overlap(parseLines(input))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day05.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
