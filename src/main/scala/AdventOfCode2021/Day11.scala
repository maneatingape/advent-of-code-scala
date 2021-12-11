package AdventOfCode2021

object Day11:
  val directions = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  case class Grid(energy: Map[Point, Int]):
    def neighbours(point: Point): Seq[Point] = directions.map(point.updated).filter(energy.contains)
    def incremented(point: Point): Grid = Grid(energy.updated(point, energy(point) + 1))
    def zeroed(point: Point): Grid = Grid(energy.updated(point, 0))

  def parseGrid(input: Seq[String]): (Grid, Int) =
    val energy = input.map(_.map(_.asDigit))
    val points = Seq.tabulate(energy.head.size, energy.size)((x, y) => Point(x, y) -> energy(y)(x))
    (Grid(points.flatten.toMap), 0)

  def step(grid: Grid, unused: Int): (Grid, Int) =
    def helper(grid: Grid, todo: Seq[Point], flashed: Set[Point]): (Grid, Int) = todo match
      case Nil => (grid, flashed.size)
      case (head :: tail) =>
        if flashed.contains(head) then helper(grid, tail, flashed)
        else if grid.energy(head) < 9 then helper(grid.incremented(head), tail, flashed)
        else helper(grid.zeroed(head), tail ++ grid.neighbours(head), flashed + head)
    helper(grid, grid.energy.keys.toSeq, Set())

  def part1(input: Seq[String]): Int = Iterator.iterate(parseGrid(input))(step).take(101).map(_._2).sum

  def part2(input: Seq[String]): Int = Iterator.iterate(parseGrid(input))(step).indexWhere(_._2 == 100)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day11.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
