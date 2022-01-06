package AdventOfCode2018

import scala.annotation.tailrec

class Day17(input: Seq[String]):
  case class Ground(top: Int, bottom: Int, clay: Set[Point])

  case class Point(x: Int, y: Int):
    def left: Point = Point(x - 1, y)
    def right: Point = Point(x + 1, y)
    def below: Point = Point(x, y + 1)

  case class Span(start: Point, end: Point):
    def points: Seq[Point] = for x <- start.x to end.x yield Point(x, start.y)

  def parse(input: Seq[String]): Ground =
    val vertical = """x=(\d+), y=(\d+)..(\d+)""".r
    val horizontal = """y=(\d+), x=(\d+)..(\d+)""".r
    val clay = input.toSet.flatMap {
      case vertical(x, start, end) => (start.toInt to end.toInt).map(y => Point(x.toInt, y))
      case horizontal(y, start, end) => (start.toInt to end.toInt).map(x => Point(x, y.toInt))
    }
    Ground(clay.map(_.y).min, clay.map(_.y).max, clay)

  @tailrec
  private def waterfall(ground: Ground, spans: Set[Span], stopped: Set[Point], moving: Set[Point]): (Set[Point], Set[Point]) =
    def blocked(point: Point) = stopped.contains(point) || ground.clay.contains(point)

    val newMoving = spans
      .flatMap { span => Seq(span.start.below, span.end.below) }
      .flatMap { edge =>
        Iterator.iterate(edge)(_.below).takeWhile(point => !moving.contains(point) && !blocked(point) && point.y <= ground.bottom)
      }
    val newSpans = newMoving.map(point => Span(point, point))

    val expandedSpans = (spans ++ newSpans).map { case Span(start, end) =>
      val nextStart = Iterator.iterate(start)(_.left).dropWhile(next => blocked(next.below) && !blocked(next.left)).next()
      val nextEnd = Iterator.iterate(end)(_.right).dropWhile(next => blocked(next.below) && !blocked(next.right)).next()
      Span(nextStart, nextEnd)
    }
    val stoppedSpans = expandedSpans.filter { case span @ Span(start, end) =>
      blocked(start.left) && blocked(end.right) && span.points.map(_.below).forall(blocked)
    }

    val nextSpans = expandedSpans -- stoppedSpans
    val nextStopped = stopped ++ stoppedSpans.flatMap(_.points)
    val nextMoving = moving ++ newMoving

    if spans == nextSpans && stopped == nextStopped then (spans.filter(_.start.y >= ground.top).flatMap(_.points), stopped)
    else waterfall(ground, nextSpans, nextStopped, nextMoving)
  end waterfall

  private lazy val result =
    val (moving, stopped) = waterfall(parse(input), Set(Span(Point(500, 1), Point(500, 1))), Set(), Set())
    (moving.size, stopped.size)

  def part1: Int = result._1 + result._2

  def part2: Int = result._2

object Day17:
  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day17.txt").getLines().toSeq
    val day17 = Day17(data)
    println(day17.part1)
    println(day17.part2)
