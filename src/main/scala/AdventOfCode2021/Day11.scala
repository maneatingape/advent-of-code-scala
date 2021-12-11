package AdventOfCode2021

object Day11:
  val directions = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  case class Grid(energy: Map[Point, Int]):
    def neighbours(point: Point): Seq[Point] = directions.map(point.updated).filter(energy.contains)
    def incremented(point: Point): Grid = Grid(energy.updated(point, energy(point) + 1))
    def zeroed(point: Point): Grid = Grid(energy.updated(point, 0))

  case class State(grid: Grid, flashes: Int)

  def parse(input: Seq[String]): State =
    val points = Seq.tabulate(input.head.size, input.size)((x, y) => Point(x, y) -> input(y)(x).asDigit)
    State(Grid(points.flatten.toMap), 0)

  def step(current: State): State =
    def helper(grid: Grid, todo: Seq[Point], flashed: Set[Point]): State = todo match
      case Nil => State(grid, flashed.size)
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
