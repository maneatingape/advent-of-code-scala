package AdventOfCode2022

object Day14:
  val start = Point(500, 0)
  val order = Seq((0, 1), (-1, 1), (1, 1))

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  def parse(input: Seq[String]): (Set[Point], Int) =
    val grid = input.toSet.flatMap { line =>
      line.split("\\D+").map(_.toInt).sliding(4, 2).flatMap { case Array(x1, y1, x2, y2) =>
        for
          x <- x1 to x2 by (if x1 < x2 then 1 else -1)
          y <- y1 to y2 by (if y1 < y2 then 1 else -1)
        yield Point(x, y)
      }
    }
    (grid, grid.map(_.y).max + 1)

  def fall(grid: Set[Point], floor: Int, sand: Point): Point =
    if sand.y == floor then sand else
      val next = order.map(sand.delta).filterNot(grid.contains)
      if next.isEmpty then sand else fall(grid, floor, next.head)

  def part1(input: Seq[String]): Int =
    val (initial, floor) = parse(input)

    def helper(grid: Set[Point]): Int =
      val sand = fall(grid, floor, start)
      if sand.y == floor then grid.size - initial.size else helper(grid + sand)

    helper(initial)
  end part1

  def part2(input: Seq[String]): Int =
    val (initial, floor) = parse(input)

    def helper(grid: Set[Point], index: Int): Int =
      val sand = fall(grid, floor, start)
      if sand == start then index else helper(grid + sand, index + 1)

    helper(initial, 1)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day14.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
