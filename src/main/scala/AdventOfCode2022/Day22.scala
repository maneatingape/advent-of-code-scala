package AdventOfCode2022

object Day22:
  val (right, down, left, up) = (Point(1, 0), Point(0, 1), Point(-1, 0), Point(0, -1))

  type State = (Point, Point)

  enum Tile:
    case Open, Solid, Wrap

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def clockwise: Point = Point(-y, x)
    def counterClockwise: Point = Point(y, -x)

  def parse(input: Seq[String]): (Map[Point, Tile], String) =
    val points = for
      (row, y) <- input.dropRight(2).zipWithIndex
      (cell, x) <- row.zipWithIndex
      if cell != ' '
    yield Point(x, y) -> (if cell == '.' then Tile.Open else Tile.Solid)
    (points.toMap.withDefaultValue(Tile.Wrap), input.last)

  def follow(tiles: Map[Point, Tile], path: String, handleWrap: State => State): Int =
    val numbers = path.split("\\D+").map(_.toInt).toSeq
    val letters = path.split("\\d+").toSeq
    val moves = numbers.zip(letters)

    val initial = (Point(50, 0), Point(1, 0))
    val (position, direction) = moves.foldLeft(initial) { case ((position, direction), (number, letter)) =>
      val nextDirection = letter match
        case "L" => direction.counterClockwise
        case "R" => direction.clockwise
        case _ => direction

      (1 to number).foldLeft((position, nextDirection)) { case ((position, direction), _) =>
        val next = position + direction
        tiles(next) match
          case Tile.Open => (next, direction)
          case Tile.Solid => (position, direction)
          case Tile.Wrap =>
            val (wrapPosition, wrapDirection) = handleWrap(position, direction)
            if tiles(wrapPosition) == Tile.Open then (wrapPosition, wrapDirection) else (position, direction)
      }
    }

    1000 * (position.y + 1) + 4 * (position.x + 1) + Seq(right, down, left, up).indexOf(direction)
  end follow

  def part1(input: Seq[String]): Int =
    val (tiles, path) = parse(input)

    val minX = tiles.keys.groupMapReduce(_.y)(_.x)(_ min _)
    val maxX = tiles.keys.groupMapReduce(_.y)(_.x)(_ max _)
    val minY = tiles.keys.groupMapReduce(_.x)(_.y)(_ min _)
    val maxY = tiles.keys.groupMapReduce(_.x)(_.y)(_ max _)

    def handleWrap(position: Point, direction: Point): State = direction match
      case `right` => position.copy(x = minX(position.y)) -> right
      case `left` => position.copy(x = maxX(position.y)) -> left
      case `down` => position.copy(y = minY(position.x)) -> down
      case `up` => position.copy(y = maxY(position.x)) -> up

    follow(tiles, path, handleWrap)
  end part1

  def part2(input: Seq[String]): Int =
    // Cube faces:
    //  BA
    //  C
    // ED
    // F
    def handleWrap(position: Point, direction: Point): State =
      val (cubeX, cubeY) = (position.x / 50, position.y / 50)
      val (modX, modY) = (position.x % 50, position.y % 50)
      (cubeX, cubeY, direction) match
        case (2, 0, `up`) => Point(modX, 199) -> up           // A to F
        case (2, 0, `down`) => Point(99, 50 + modX) -> left   // A to C
        case (2, 0, `right`) => Point(99, 149 - modY) -> left // A to D
        case (1, 0, `up`) => Point(0, 150 + modX) -> right    // B to F
        case (1, 0, `left`) => Point(0, 149 - modY) -> right  // B to E
        case (1, 1, `left`) => Point(modY, 100) -> down       // C to E
        case (1, 1, `right`) => Point(100 + modY, 49) -> up   // C to A
        case (1, 2, `down`) => Point(49, 150 + modX) -> left  // D to F
        case (1, 2, `right`) => Point(149, 49 - modY) -> left // D to A
        case (0, 2, `up`) => Point(50, 50 + modX) -> right    // E to C
        case (0, 2, `left`) => Point(50, 49 - modY) -> right  // E to B
        case (0, 3, `down`) => Point(100 + modX, 0) -> down   // F to A
        case (0, 3, `left`) => Point(50 + modY, 0) -> down    // F to B
        case (0, 3, `right`) => Point(50 + modY, 149) -> up   // F to D

    val (tiles, path) = parse(input)
    follow(tiles, path, handleWrap)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
