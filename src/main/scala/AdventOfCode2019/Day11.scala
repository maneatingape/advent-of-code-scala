package AdventOfCode2019

import Day09.IntCode
import Day09.IntCode._
import Day11.Direction._

object Day11:
  enum Direction:
    case Up, Down, Left, Right

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  val clockwise = Map(Up -> Right, Right -> Down, Down -> Left, Left -> Up)
  val antiClockwise = Map(Up -> Left, Left -> Down, Down -> Right, Right -> Up)
  val deltas = Map(Up -> (0, -1), Down -> (0, 1), Left -> (-1, 0), Right -> (1, 0))

  def paint(intCode: IntCode, direction: Direction, position: Point, panels: Map[Point, Long]): Map[Point, Long] =
    val first = intCode.withInput(panels(position)).nextOutput
    first.result match
      case Halted => panels
      case Output(color) =>
        val second = first.nextOutput
        val Output(turn) = second.result

        val nextDirection = if turn == 1 then clockwise(direction) else antiClockwise(direction)
        val nextPosition = position.updated.tupled(deltas(nextDirection))
        val nextPanels = panels.updated(position, color)

        paint(second, nextDirection, nextPosition, nextPanels)
  end paint

  def part1(memory: Seq[Long]): Long = paint(IntCode(memory), Up, Point(0, 0), Map().withDefaultValue(0L)).size

  def part2(memory: Seq[Long]): Map[Point, Long] = paint(IntCode(memory), Up, Point(0, 0), Map(Point(0, 0) -> 1L).withDefaultValue(0L))

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
