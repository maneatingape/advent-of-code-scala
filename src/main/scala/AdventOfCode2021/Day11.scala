package AdventOfCode2021

object Day11:
  val directions = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  type Grid = Map[Point, Int]
  extension (grid: Grid)
    def neighbours(point: Point): Seq[Point] = directions.map(point.delta).filter(grid.contains)
    def flashes: Int = grid.values.count(_ == 0)

  def parse(input: Seq[String]): Grid =
    Seq.tabulate(input.head.size, input.size)((x, y) => Point(x, y) -> input(y)(x).asDigit).flatten.toMap

  def step(current: Grid): Grid =
    def helper(grid: Grid, todo: Seq[Point], flashed: Set[Point]): Grid = todo match
      case Nil => grid
      case (head :: tail) =>
        if flashed.contains(head) then helper(grid, tail, flashed)
        else if grid(head) < 9 then helper(grid.updated(head, grid(head) + 1), tail, flashed)
        else helper(grid.updated(head, 0), tail ++ grid.neighbours(head), flashed + head)
    helper(current, current.keys.toSeq, Set())

  def part1(input: Seq[String]): Int = Iterator.iterate(parse(input))(step).take(101).map(_.flashes).sum

  def part2(input: Seq[String]): Int = Iterator.iterate(parse(input))(step).indexWhere(_.flashes == 100)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day11.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
