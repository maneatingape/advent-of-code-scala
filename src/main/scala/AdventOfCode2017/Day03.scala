package AdventOfCode2017

object Day03:
  val adjacent = for y <- -1 to 1; x <- -1 to 1 if !(x == 0 && y == 0) yield (x, y)

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def neighbours: Seq[Point] = adjacent.map(delta)

  def position(input: Int): Point =
    val n = Iterator.iterate(1)(_ + 2).dropWhile(n => n * n < input).next()
    val base = input - (n - 2) * (n - 2) - 1
    val size = n - 1
    val half = size / 2
    val quadrant = base / size
    val offset = base % size
    quadrant match
      case 0 => Point(half, offset + 1 - half)
      case 1 => Point(half - 1 - offset, half)
      case 2 => Point(-half, half - 1 - offset)
      case 3 => Point(offset + 1 - half, -half)
  end position

  def part1(input: Int): Int =
    val point = position(input)
    point.x.abs + point.y.abs

  def part2(input: Int): Int =
    def helper(n: Int, squares: Map[Point, Int]): Int =
      val point = position(n)
      val result = point.neighbours.flatMap(squares.get).sum
      if result > input then result
      else helper(n + 1, squares.updated(point, result))

    helper(2, Map(Point(0, 0) -> 1))
  end part2

  def main(args: Array[String]): Unit =
    val data = 312051
    println(part1(data))
    println(part2(data))
