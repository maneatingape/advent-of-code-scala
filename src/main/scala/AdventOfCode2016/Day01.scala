package AdventOfCode2016

import scala.annotation.tailrec

object Day01:
  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def manhattan: Int = x.abs + y.abs
    def cw: Point = Point(-y, x)
    def ccw: Point = Point(y, -x)

  case class State(locations: Seq[Point], direction: Point)

  def path(input: Seq[String]): Seq[Point] =
    val start = State(Seq(Point(0, 0)), Point(0, -1))
    input.scanLeft(start) { case (State(positions, direction), next) =>
      val nextDirection = if next.head == 'R' then direction.cw else direction.ccw
      val points = Iterator.iterate(positions.last)(_ + nextDirection).drop(1).take(next.tail.toInt).toSeq
      State(points, nextDirection)
    }
    .flatMap(_.locations)

  def part1(input: Seq[String]): Int = path(input).last.manhattan

  def part2(input: Seq[String]): Int =
    @tailrec
    def helper(remaining: Seq[Point], visited: Set[Point]): Int =
      if visited.contains(remaining.head) then remaining.head.manhattan
      else helper(remaining.tail, visited + remaining.head)

    helper(path(input), Set())
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day01.txt").mkString.trim.split(", ").toSeq
    println(part1(data))
    println(part2(data))
