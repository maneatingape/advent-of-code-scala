package AdventOfCode2022

import scala.annotation.tailrec

object Day17:
  val shapes = Seq(
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),
    Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)),
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2)),
    Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)),
    Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1)))

  extension (shape: Set[Point])
    def move(dx: Int, dy: Int): Set[Point] = shape.map(p => Point(p.x + dx, p.y + dy))
    def canMove(grid: Set[Point]): Boolean = shape.forall(p => p.x > 0 && p.x < 8 && !grid.contains(p))

  case class Point(x: Int, y: Int)
  case class State(grid: Set[Point], shapeIndex: Int, jetIndex: Int, height: Int)

  @tailrec
  def fall(grid: Set[Point], shape: Set[Point], jets: String, jetIndex: Int): (Set[Point], Int) =
    val jet = jets(jetIndex % jets.length)
    val first = if jet == '>' then shape.move(1, 0) else shape.move(-1, 0)
    val second = if first.canMove(grid) then first else shape
    val third = second.move(0, -1)
    if third.canMove(grid) then fall(grid, third, jets, jetIndex + 1) else (second, jetIndex + 1)

  def step(jets: String)(state: State): State =
    val State(grid, shapeIndex, jetIndex, height) = state
    val initialShape = shapes(shapeIndex % shapes.size).move(3, height + 4)
    val (nextShape, nextJetIndex) = fall(grid, initialShape, jets, jetIndex)
    State(grid ++ nextShape, shapeIndex + 1, nextJetIndex, height.max(nextShape.map(_.y).max))

  def simulate(jets: String): Iterator[Int] =
    val initial = State(Set.tabulate(8)(Point(_, 0)), 0, 0, 0)
    Iterator.iterate(initial)(step(jets)).map(_.height)

  def part1(input: String): Int = simulate(input).drop(2022).next()

  def part2(input: String): Long =
    val guess = 1000
    val height = simulate(input).slice(1, 10 * guess).toSeq
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
