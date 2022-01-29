package AdventOfCode2021

object Day09:
  val directions = Seq((1, 0), (-1, 0), (0, -1), (0, 1))

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  type Grid = Map[Point, Int]
  extension (grid: Grid)
    def neighbours(point: Point): Seq[Point] = directions.map(point.delta).filter(grid.contains)
    def lowest(point: Point): Boolean = neighbours(point).map(grid).forall(_ > grid(point))
    def lowestPoints: Seq[(Grid, Point)] = grid.keys.toSeq.filter(lowest).map(grid -> _)

  def parse(input: Seq[String]): Grid =
    Seq.tabulate(input.head.size, input.size)((x, y) => Point(x, y) -> input(y)(x).asDigit).flatten.toMap

  def risk(grid: Grid, point: Point): Int = 1 + grid(point)

  def basinSize(grid: Grid, lowest: Point): Int =
    def visit(visited: Set[Point], point: Point): Set[Point] =
      if visited.contains(point) || grid(point) == 9 then visited
      else grid.neighbours(point).foldLeft(visited + point)(visit)
    visit(Set(), lowest).size

  def part1(input: Seq[String]): Int = parse(input).lowestPoints.map(risk).sum

  def part2(input: Seq[String]): Int = parse(input).lowestPoints.map(basinSize).sorted.takeRight(3).product

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day09.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
