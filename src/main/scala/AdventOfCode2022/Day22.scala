package AdventOfCode2022

object Day22:
  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def clockwise: Point = Point(-y, x)
    def counterClockwise: Point = Point(y, -x)
    def score: Int = 1000 * (y + 1) + 4 * (x + 1)

  case class Vec(x: Int, y: Int, z: Int):
    def *(k: Int): Vec = Vec(x * k, y * k, z * k)
    def +(b: Vec): Vec = Vec(x + b.x, y + b.y, z + b.z)
    def cross(b: Vec): Vec = Vec(y * b.z - z * b.y, z * b.x - x * b.z, x * b.y - y * b.x)

  case class Info(point: Point, i: Vec, j: Vec, k: Vec)

  def parseTiles(input: Seq[String]): Map[Point, Boolean] =
    val points = for
      (row, y) <- input.dropRight(2).zipWithIndex
      (cell, x) <- row.zipWithIndex
      if cell != ' '
    yield Point(x, y) -> (cell == '.')
    points.toMap

  def parseMoves(input: Seq[String]): Seq[Char] =
    val tokens = input.last.replace("L", " L ").replace("R", " R ").split(" ").toSeq
    tokens.flatMap(token => if token.head.isDigit then "F" * token.toInt else token)

  def part1(input: Seq[String]): Int =
    val tiles = parseTiles(input)
    val moves = parseMoves(input)

    val minX = tiles.keys.groupMapReduce(_.y)(_.x)(_ min _)
    val maxX = tiles.keys.groupMapReduce(_.y)(_.x)(_ max _)
    val minY = tiles.keys.groupMapReduce(_.x)(_.y)(_ min _)
    val maxY = tiles.keys.groupMapReduce(_.x)(_.y)(_ max _)

    val facing = Seq(Point(1, 0), Point(0, 1), Point(-1, 0), Point(0, -1))
    val Seq(right, down, left, up) = facing
    val topLeft = tiles.keys.filter(_.y == 0).minBy(_.x)

    val (position, direction) = moves.foldLeft((topLeft, right)) { case ((position, direction), move) =>
      move match
        case 'L' => (position, direction.counterClockwise)
        case 'R' => (position, direction.clockwise)
        case _ =>
          val next = position + direction
          tiles.get(next) match
            case Some(true) => (next, direction)
            case Some(false) => (position, direction)
            case None =>
              val wrapPosition = direction match
                case `right` => position.copy(x = minX(position.y))
                case `left` => position.copy(x = maxX(position.y))
                case `down` => position.copy(y = minY(position.x))
                case `up` => position.copy(y = maxY(position.x))
              if tiles(wrapPosition) then (wrapPosition, direction) else (position, direction)
    }

    position.score + facing.indexOf(direction)
  end part1

  def part2(input: Seq[String], block: Int): Int =
    val tiles = parseTiles(input)
    val moves = parseMoves(input)

    val scaleIJ = block - 1
    val scaleK = block + 1
    val startingPosition = Vec(-scaleIJ, -scaleIJ, -scaleK)
    val startingDirection = Vec(2, 0, 0)

    val topLeft = tiles.keys.filter(_.y == 0).minBy(_.x)
    val start = Info(topLeft, Vec(1, 0, 0), Vec(0, 1, 0), Vec(0, 0, 1))

    val todo = collection.mutable.Queue(start)
    val visited = collection.mutable.Set(topLeft)
    val points = collection.mutable.Map[Vec, Info]()

    while todo.nonEmpty do
      val Info(offset, i, j, k) = todo.dequeue()

      for x <- 0 until block do
        for y <- 0 until block do
          val key = (i * (2 * x - scaleIJ)) + (j * (2 * y - scaleIJ)) + (k * -scaleK) // Scale by 2 to keep points integer
          points(key) = Info(offset + Point(x, y), i, j, k)

      val neighbours = Seq(
        Info(offset + Point(-block, 0), j.cross(i), j, j.cross(k)), // Left
        Info(offset + Point(block, 0),  i.cross(j), j, k.cross(j)), // Right
        Info(offset + Point(0, -block), i, j.cross(i), k.cross(i)), // Up
        Info(offset + Point(0, block),  i, i.cross(j), i.cross(k))) // Down

      neighbours.foreach { next =>
        if tiles.contains(next.point) && !visited.contains(next.point) then
          todo += next
          visited += next.point
      }
    end while

    val (position, direction) = moves.foldLeft((startingPosition, startingDirection)) { case ((position, direction), move) =>
      move match
        case 'L' => (position, direction.cross(points(position).k))
        case 'R' => (position, points(position).k.cross(direction))
        case _ =>
          val next = position + direction
          if points.contains(next) then
            if tiles(points(next).point) then (next, direction) else (position, direction)
          else
            val wrapDirection = points(position).k * 2 // This is the fun part
            val wrapPosition = next + wrapDirection
            if tiles(points(wrapPosition).point) then (wrapPosition, wrapDirection) else (position, direction)
    }

    val Info(point, i, j, k) = points(position)
    point.score + Seq(i * 2, j * 2, i * -2, j * -2).indexOf(direction)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data, 50))
