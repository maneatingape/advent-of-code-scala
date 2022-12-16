package AdventOfCode2017

object Day22:
  sealed trait Node
  case object Clean extends Node
  case object Weakened extends Node
  case object Infected extends Node
  case object Flagged extends Node

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def cw: Point = Point(-y, x)
    def ccw: Point = Point(y, -x)

  case class State(grid: Map[Point, Node], location: Point, direction: Point, infected: Int):
    def next(node: Node, direction: Point, infect: Int): State =
      State(grid.updated(location, node), location + direction, direction, infected + infect)

  def parse(input: Seq[String]): State =
    val points = for y <- input.indices; x <- input.indices if input(y)(x) == '#' yield Point(x, y)
    val start = points.map(_ -> Infected).toMap.withDefaultValue(Clean)
    State(start, Point(input.size / 2, input.size / 2), Point(0, -1), 0)

  def part1(input: Seq[String]): Int =
    def step(state: State): State = state.grid(state.location) match
      case Clean => state.next(Infected, state.direction.ccw, 1)
      case Infected => state.next(Clean, state.direction.cw, 0)
      case _ => throw MatchError("Unreachable")

    Iterator.iterate(parse(input))(step).drop(10000).next().infected
  end part1

  def part2(input: Seq[String]): Int =
    def step(state: State): State = state.grid(state.location) match
      case Clean => state.next(Weakened, state.direction.ccw, 0)
      case Weakened => state.next(Infected, state.direction, 1)
      case Infected => state.next(Flagged, state.direction.cw, 0)
      case Flagged => state.next(Clean, state.direction.cw.cw, 0)

    Iterator.iterate(parse(input))(step).drop(10000000).next().infected
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
