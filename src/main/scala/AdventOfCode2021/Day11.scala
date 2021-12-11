package AdventOfCode2021

object Day11:
  val directions = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  case class Grid(energy: Map[Point, Int]):
    def neighbours(point: Point): Seq[Point] = directions.map(point.updated).filter(energy.contains)
    def incremented(point: Point): Grid = Grid(energy.updated(point, energy(point) + 1))
    def zeroed(point: Point): Grid = Grid(energy.updated(point, 0))

  case class Step(grid: Grid, flashes: Int)

  def parse(input: Seq[String]): Step =
    val energy = input.map(_.map(_.asDigit))
    val points = Seq.tabulate(energy.head.size, energy.size)((x, y) => Point(x, y) -> energy(y)(x))
    Step(Grid(points.flatten.toMap), 0)

  def step(current: Step): Step =
    def helper(grid: Grid, todo: Seq[Point], flashed: Set[Point]): Step = todo match
      case Nil => Step(grid, flashed.size)
      case (head :: tail) =>
        if flashed.contains(head) then helper(grid, tail, flashed)
        else if grid.energy(head) < 9 then helper(grid.incremented(head), tail, flashed)
        else helper(grid.zeroed(head), tail ++ grid.neighbours(head), flashed + head)
    helper(current.grid, current.grid.energy.keys.toSeq, Set())

  def part1(input: Seq[String]): Int = Iterator.iterate(parse(input))(step).take(101).map(_.flashes).sum

  def part2(input: Seq[String]): Int = Iterator.iterate(parse(input))(step).indexWhere(_.flashes == 100)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day11.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
