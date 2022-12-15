package AdventOfCode2022

import scala.annotation.tailrec

object Day15:
  case class Point(x: Int, y :Int):
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs

  case class Interval(min: Int, max: Int):
    def touch(other: Interval): Boolean =
      (other.min <= min && min - other.max <= 1) ||
        (min <= other.min && other.min - max <= 1)
    def merge(other: Interval): Interval = Interval(min.min(other.min), max.max(other.max))
    def size: Int = max - min + 1

  def parse(input: Seq[String]): Seq[(Point, Point, Int)] = input.map { line =>
    val Array(x1, y1, x2, y2) = line.split("[^-\\d]+").tail.map(_.toInt)
    val sensor = Point(x1, y1)
    val beacon = Point(x2, y2)
    (sensor, beacon, sensor.manhattan(beacon))
  }

  def build(data: Seq[(Point, Point, Int)], y: Int): Seq[Interval] =
    data.foldLeft(Seq.empty[Interval]) { case (intervals, (sensor, beacon, range)) =>
      val extra = range - (sensor.y - y).abs
      if extra < 0 then intervals else
        val next = Interval(sensor.x - extra, sensor.x + extra)
        val (in, out) = intervals.partition(_.touch(next))
        in.foldLeft(next)(_ merge _) +: out
    }

  def part1(input: Seq[String], row: Int): Int =
    val data = parse(input)
    val out = build(data, row).map(_.size).sum
    val in = data.collect { case (_, beacon, _) if beacon.y == row => beacon }.toSet.size
    out - in

  def part2(input: Seq[String]): Long =
    val data = parse(input)

    @tailrec
    def helper(y: Int): Long = build(data, y) match
      case Seq(first, second) => 4000000L * (first.max.min(second.max) + 1) + y
      case _ => helper(y + 1)

    helper(0)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day15.txt").getLines().toSeq
    println(part1(data, 2000000))
    println(part2(data))
