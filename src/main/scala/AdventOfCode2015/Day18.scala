package AdventOfCode2015

object Day18:
  val adjacent = for y <- -1 to 1; x <- -1 to 1 if !(x == 0 && y == 0) yield (x, y)

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def neighbours: Seq[Point] = adjacent.map(delta)

  def parse(input: Seq[String]): Set[Point] =
    Set.tabulate(100, 100)(Point).flatten.filter(p => input(p.y)(p.x) == '#')

  def step(grid: Set[Point]): Set[Point] =
    def rule(point: Point) =
      val active = grid.contains(point)
      val count = point.neighbours.count(grid.contains)
      count == 3 || (active && count == 2)
    Set.tabulate(100, 100)(Point).flatten.filter(rule)

  def corners(grid: Set[Point]): Set[Point] =
    step(grid) ++ Set(Point(0, 0), Point(0, 99), Point(99, 0), Point(99, 99))

  def part1(input: Seq[String]): Int = Iterator.iterate(parse(input))(step).drop(100).next().size

  def part2(input: Seq[String]): Int = Iterator.iterate(parse(input))(corners).drop(100).next().size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
