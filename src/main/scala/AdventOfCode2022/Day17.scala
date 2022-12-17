package AdventOfCode2022

import scala.annotation.tailrec

object Day17:
  val shapes = Seq(
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),
    Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)),
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2)),
    Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)),
    Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1)))

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)

  case class State(grid: Set[Point], wind: String, shapeIndex: Int, windIndex: Int, height: Int)

  @tailrec
  def fall(grid: Set[Point], shape: Set[Point], wind: String, windIndex: Int): (Int, Set[Point]) =
    var next = shape

    val gust = wind(windIndex % wind.size)
    val shift = if gust == '>' then Point(1, 0) else Point(-1, 0)
    var test = shape.map(_ + shift)
    if test.forall(p => p.x > 0 && p.x < 8 && !grid.contains(p)) then next = test

    test = next.map(_ + Point(0, -1))
    if test.exists(grid.contains) then (windIndex + 1, next) else fall(grid, test, wind, windIndex + 1)
  end fall

  def step(state: State): State =
    val State(grid, wind, shapeIndex, windIndex, height) = state
    val initial = Point(3, height + 4)
    val next = shapes(shapeIndex % shapes.size).map(_ + initial)
    val (nextWindIndex, foo) = fall(grid, next, wind, windIndex)

    State(grid ++ foo, wind, shapeIndex + 1, nextWindIndex, height.max(foo.map(_.y).max))
  end step

  def simulate(input: String): Iterator[State] =
    val initial = State(Set.tabulate(8)(Point(_, 0)), input, 0, 0, 0)
    Iterator.iterate(initial)(step)

  def part1(input: String): Int = simulate(input).drop(2022).next().height

  def part2(input: String): Long =
    val guess = 1000
    val height = simulate(input).drop(1).take(10 * guess).map(_.height).toSeq
    val delta = height.sliding(2).map { case Seq(a, b) => b - a }.toSeq
    val index = delta.lastIndexOfSlice(delta.takeRight(guess), delta.size - guess - 1)
    val cycleHeight = height(delta.size - guess) - height(index)
    val cycleWidth = delta.size - guess - index
    val offset = 1000000000000L - 1 - index
    val quotient = offset / cycleWidth
    val remainder = offset % cycleWidth
    (quotient * cycleHeight) + height(index + remainder.toInt)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day17.txt").mkString.trim
    println(part1(data))
    println(part2(data))
