package AdventOfCode2022

import scala.annotation.tailrec

object Day15:
  case class Point(x: Int, y :Int)
  case class Info(sensor: Point, beacon: Point, manhattan: Int)

  case class Interval(min: Int, max: Int):
    def size: Int = max - min + 1
    def merge(other: Interval): Interval = Interval(min.min(other.min), max.max(other.max))
    def touching(other: Interval): Boolean = if other.min < min then min - other.max <= 1 else other.min - max <= 1

  def parse(input: Seq[String]): Seq[Info] = input.map { line =>
    val Array(x1, y1, x2, y2) = line.split("[^-\\d]+").tail.map(_.toInt)
    Info(Point(x1, y1), Point(x2, y2), (x1 - x2).abs + (y1 - y2).abs)
  }

  def build(info: Seq[Info], y: Int): Seq[Interval] = info.foldLeft(Seq.empty[Interval]) { case (intervals, info) =>
    val extra = info.manhattan - (info.sensor.y - y).abs
    if extra < 0 then intervals else
      val next = Interval(info.sensor.x - extra, info.sensor.x + extra)
      val (in, out) = intervals.partition(_.touching(next))
      in.foldLeft(next)(_ merge _) +: out
  }

  def part1(input: Seq[String], row: Int): Int =
    val info = parse(input)
    build(info, row).head.size - info.map(_.beacon).filter(_.y == row).distinct.size

  def part2(input: Seq[String]): Long =
    @tailrec
    def helper(info: Seq[Info], y: Int): Long = build(info, y) match
      case Seq(first, second) => 4000000L * (first.max.min(second.max) + 1) + y
      case _ => helper(info, y + 1)

    helper(parse(input), 0)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day15.txt").getLines().toSeq
    println(part1(data, 2000000))
    println(part2(data))
