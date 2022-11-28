package AdventOfCode2019

import Day09.IntCode
import Day09.IntCode._

object Day11:
  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def cw: Point = Point(-y, x)
    def ccw: Point = Point(y, -x)

  def paint(intCode: IntCode, direction: Point, position: Point, panels: Map[Point, Long]): Map[Point, Long] =
    val first = intCode.withInput(panels(position)).nextOutput
    first.result match
      case Output(color) =>
        val second = first.nextOutput
        val Output(turn) = second.result: @unchecked
        val nextDirection = if turn == 1 then direction.cw else direction.ccw
        paint(second, nextDirection, position + nextDirection, panels.updated(position, color))
      case _ => panels
  end paint

  def part1(memory: Seq[Long]): Long = paint(IntCode(memory), Point(0, -1), Point(0, 0), Map().withDefaultValue(0L)).size

  def part2(memory: Seq[Long]): Map[Point, Long] = paint(IntCode(memory), Point(0, -1), Point(0, 0), Map(Point(0, 0) -> 1L).withDefaultValue(0L))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day11.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))

    val panels = part2(data)
    val minX = panels.keys.map(_.x).min
    val maxX = panels.keys.map(_.x).max
    val minY = panels.keys.map(_.y).min
    val maxY = panels.keys.map(_.y).max

    for y <- minY to maxY do
      println()
      for x <- minX to maxX do
        print(if panels(Point(x, y)) == 1 then '#' else ' ')
  end main
