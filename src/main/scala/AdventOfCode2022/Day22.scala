package AdventOfCode2022

object Day22:
  val (right, down, left, up) = (Point(1, 0), Point(0, 1), Point(-1, 0), Point(0, -1))

  enum Tile:
    case Open, Solid, Wrap
  import Tile._

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def clockwise: Point = Point(-y, x)
    def counterClockwise: Point = Point(y, -x)

  case class State(position: Point, direction: Point)

  def parse(input: Seq[String]): Map[Point, Tile] =
    val points = for
      (row, y) <- input.dropRight(2).zipWithIndex
      (cell, x) <- row.zipWithIndex
      if cell != ' '
    yield Point(x, y) -> (if cell == '.' then Open else Solid)
    points.toMap.withDefaultValue(Wrap)

  def follow(points: Map[Point, Tile], path: String, handleWrap: State => State): Int =
    val numbers = path.split("\\D+").map(_.toInt).toSeq
    val letters = path.split("\\d+").toSeq
    val moves = numbers.zip(letters)

    val initial = State(Point(50, 0), Point(1, 0))
    val result = moves.foldLeft(initial) { case (state, (number, letter)) =>
      val nextDirection = letter match
        case "L" => state.direction.counterClockwise
        case "R" => state.direction.clockwise
        case _ => state.direction

      (1 to number).foldLeft(State(state.position, nextDirection)) { (state, _) =>
        val State(position, direction) = state
        val next = position + direction
        points(next) match
          case Open => State(next, direction)
          case Solid => state
          case Wrap => handleWrap(state)
      }
    }

    val facing = Seq(right, down, left, up)
    1000 * (result.position.y + 1) + 4 * (result.position.x + 1) + facing.indexOf(result.direction)
  end follow

  def part1(input: Seq[String]): Int =
    val tiles = parse(input)

    val valid = tiles.filterNot((k, v) => v == Wrap).keySet
    val minX = valid.groupMapReduce(_.y)(_.x)(_ min _)
    val maxX = valid.groupMapReduce(_.y)(_.x)(_ max _)
    val minY = valid.groupMapReduce(_.x)(_.y)(_ min _)
    val maxY = valid.groupMapReduce(_.x)(_.y)(_ max _)

    def handleWrap(state: State): State =
      val State(position, direction) = state
      val nextPosition = direction match
        case `right` => Point(minX(position.y), position.y)
        case `left` => Point(maxX(position.y), position.y)
        case `down` => Point(position.x, minY(position.x))
        case `up` => Point(position.x, maxY(position.x))
      if tiles(nextPosition) == Open then State(nextPosition, direction) else state

    follow(tiles, input.last, handleWrap)
  end part1

  def part2(input: Seq[String]): Int =
    val tiles = parse(input)

    def handleWrap(state: State): State =
      val State(position, direction) = state
      val (cubeX, cubeY) = (position.x / 50, position.y / 50)
      val (modX, modY) = (position.x % 50, position.y % 50)

      // Cube faces:
      //  BA
      //  C
      // ED
      // F
      val (nextPosition, nextDirection) = (cubeX, cubeY, direction) match
        case (2, 0, `up`) => Point(modX, 199) -> up               // A to F
        case (2, 0, `down`) => Point(99, 50 + modX) -> left       // A to C
        case (2, 0, `right`) => Point(99, 149 - modY) -> left     // A to D
        case (1, 0, `up`) => Point(0, 150 + modX) -> right        // B to F
        case (1, 0, `left`) => Point(0, 149 - modY) -> right      // B to E
        case (1, 1, `left`) => Point(modY, 100) -> down           // C to E
        case (1, 1, `right`) => Point(100 + modY, 49) -> up       // C to A
        case (1, 2, `down`) => Point(49, 150 + modX) -> left      // D to F
        case (1, 2, `right`) => Point(149, 49 - modY) -> left     // D to A
        case (0, 2, `up`) => Point(50, 50 + modX) -> right        // E to C
        case (0, 2, `left`) => Point(50, 49 - modY) -> right      // E to B
        case (0, 3, `down`) => Point(100 + modX, 0) -> down       // F to A
        case (0, 3, `left`) => Point(50 + modY, 0) -> down        // F to B
        case (0, 3, `right`) => Point(50 + modY, 149) -> up       // F to D

      if tiles(nextPosition) == Open then State(nextPosition, nextDirection) else state
    end handleWrap

    follow(tiles, input.last, handleWrap)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
