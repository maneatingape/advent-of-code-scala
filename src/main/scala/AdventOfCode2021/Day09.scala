package AdventOfCode2021

object Day09:
  val directions = Seq((1, 0), (-1, 0), (0, -1), (0, 1))

  type Grid = Seq[Seq[Int]]

  case class Point(grid: Grid, x: Int, y: Int):
    def value: Int = grid(y)(x)
    def outOfBounds: Boolean = y < 0 || y == grid.length || x < 0 || x == grid(y).length
    def neighbour(dx: Int, dy: Int): Point = Point(grid, x + dx, y + dy)

  def parseGrid(input: Seq[String]): Grid = input.map(_.map(_ - '0'))

  def lowestPoints(grid: Grid): Seq[Point] =
    def test(point: Point, dx: Int, dy: Int) =
      val neighbour = point.neighbour(dx, dy)
      neighbour.outOfBounds || point.value < neighbour.value

    (0 until grid.length)
      .flatMap { y => (0 until grid(y).length).map(x => Point(grid, x, y)) }
      .filter(point => directions.forall(test(point, _, _)))
  end lowestPoints

  def basinSize(lowest: Point): Int =
    def visit(point: Point, visited: Set[Point]): Set[Point] =
      if visited.contains(point) || point.outOfBounds || point.value == 9 then
        visited
      else
        directions.foldLeft(visited + point) { case (visited, (dx, dy)) => visit(point.neighbour(dx, dy), visited) }

    visit(lowest, Set()).size
  end basinSize

  def part1(input: Seq[String]): Int = lowestPoints(parseGrid(input)).map(_.value + 1).sum

  def part2(input: Seq[String]): Int = lowestPoints(parseGrid(input)).map(basinSize).sorted.reverse.take(3).product

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day09.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
