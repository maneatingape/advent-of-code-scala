package AdventOfCode2021

object Day09:
  val sample = Array(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678")

  case class Point(x: Int, y: Int)
  type Grid = Seq[Seq[Int]]
  val directions = Seq((1, 0), (-1, 0), (0, -1), (0, 1))

  def parseGrid(input: Seq[String]): Grid = input.map(_.map(_ - '0'))

  def outOfBounds(grid: Grid, x: Int, y: Int): Boolean = y < 0 || y == grid.length || x < 0 || x == grid(y).length

  def lowestPoints(grid: Grid): Seq[Point] =
    def test(x1: Int, y1: Int, x2: Int, y2: Int) = outOfBounds(grid, x2, y2) || grid(y1)(x1) < grid(y2)(x2)

    def lowest(x: Int, y: Int): Boolean = directions.forall((dx, dy) => test(x, y, x + dx, y + dy))

    (0 until grid.length).flatMap { y =>
      (0 until grid(y).length).collect { case x if lowest(x, y) => Point(x, y) }
    }
  end lowestPoints

  def basinSize(grid: Grid, lowest: Point): Int =
    val visited = collection.mutable.LinkedHashSet[Point]()

    def visit(point: Point): Unit =
      val skip = visited.contains(point) || outOfBounds(grid, point.x, point.y) || grid(point.y)(point.x) == 9
      if !skip then
        visited += point
        directions.foreach((dx, dy) => visit(Point(point.x + dx, point.y + dy)))

    visit(lowest)
    visited.size
  end basinSize

  def part1(input: Array[String]): Int =
    val grid = parseGrid(input)
    val points = lowestPoints(grid)
    points.map(point => 1 + grid(point.y)(point.x)).sum

  def part2(input: Seq[String]): Int =
    val grid = parseGrid(input)
    val points = lowestPoints(grid)
    val basins = points.map(point => basinSize(grid, point))
    basins.sorted.reverse.take(3).product

  def main(args: Array[String]): Unit =
    //val data = sample
    val data = io.Source.fromResource("AdventOfCode2021/Day09.txt").getLines().toArray
    println(part1(data))
    println(part2(data))
