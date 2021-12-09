package AdventOfCode2021

object Day09:
  val directions = Seq((1, 0), (-1, 0), (0, -1), (0, 1))

  case class Grid(grid: Seq[Seq[Int]]):
    def height: Int = grid.length
    def width: Int = grid.head.length
    def riskAt(x: Int, y: Int): Int = 1 + grid(y)(x)
    def lowestPoints: Seq[Point] = (0 until width)
      .flatMap(x => (0 until height).map(y => Point(this, x, y)))
      .filter(_.lowest)

  case class Point(grid: Grid, x: Int, y: Int):
    def risk: Int = grid.riskAt(x, y)
    def outOfBounds: Boolean = x < 0 || x >= grid.width || y < 0 || y >= grid.height
    def neighbours: Seq[Point] = directions.map((dx, dy) => Point(grid, x + dx, y + dy))
    def lowest: Boolean = neighbours.forall(neighbour => neighbour.outOfBounds || risk < neighbour.risk)

  def parseGrid(input: Seq[String]): Grid = Grid(input.map(_.map(_ - '0')))

  def basinSize(lowest: Point): Int =
    def visit(visited: Set[Point], point: Point): Set[Point] =
      if visited.contains(point) || point.outOfBounds || point.risk == 10 then visited
      else point.neighbours.foldLeft(visited + point)(visit)
    visit(Set(), lowest).size

  def part1(input: Seq[String]): Int = parseGrid(input).lowestPoints.map(_.risk).sum

  def part2(input: Seq[String]): Int = parseGrid(input).lowestPoints.map(basinSize).sorted.reverse.take(3).product

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day09.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
