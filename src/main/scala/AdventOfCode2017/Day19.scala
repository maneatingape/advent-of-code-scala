package AdventOfCode2017

import scala.annotation.tailrec

object Day19:
  val (left, right, up, down) = (Point(-1, 0), Point(1, 0), Point(0, -1), Point(0, 1))
  val horizontal = Seq(left, right)
  val vertical = Seq(up, down)

  case class Point(x: Int, y: Int):
    def delta(other: Point): Point = Point(x + other.x, y + other.y)

  def walk(input: Seq[String]): (String, Int) =
    @tailrec
    def helper(location: Point, direction: Point, path: Seq[Char], steps: Int): (String, Int) =
      def grid(point: Point): Char = input(point.y)(point.x)
      val nextLocation = location.delta(direction)
      grid(nextLocation) match
        case ' ' => (path.mkString, steps)
        case '+' =>
          val Seq(first, second) = if horizontal.contains(direction) then vertical else horizontal
          val nextDirection = if grid(nextLocation.delta(first)) != ' ' then first else second
          helper(nextLocation, nextDirection, path, steps + 1)
        case c if c.isLetter => helper(nextLocation, direction, path.appended(c), steps + 1)
        case _ => helper(nextLocation, direction, path, steps + 1)

    helper(Point(input.head.indexOf('|'), -1), down, Seq(), 0)
  end walk

  def part1(input: Seq[String]): String = walk(input)._1

  def part2(input: Seq[String]): Int = walk(input)._2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day19.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
