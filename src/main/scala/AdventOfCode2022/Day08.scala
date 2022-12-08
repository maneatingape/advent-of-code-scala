package AdventOfCode2022

object Day08:
  val orthogonal = Seq(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)

  def parse(input: Seq[String]): Map[Point, Int] =
    val points = for y <- input.indices; x <- input.indices yield Point(x, y) -> input(y)(x).asDigit
    points.toMap

  def part1(input: Seq[String]): Int =
    val grid = parse(input)

    def helper(point: Point, tree: Int)(delta: Point): Boolean =
      val next = point + delta
      grid.get(next).forall(_ < tree && helper(next, tree)(delta))

    grid.count((k, v) => orthogonal.exists(helper(k, v)))
  end part1

  def part2(input: Seq[String]): Int =
    val grid = parse(input)

    def helper(point: Point, tree: Int)(delta: Point): Int =
      val next = point + delta
      grid.get(next).map(height => if height < tree then 1 + helper(next, tree)(delta) else 1).getOrElse(0)

    grid.map((k, v) => orthogonal.map(helper(k, v)).product).max
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day08.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
