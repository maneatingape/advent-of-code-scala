package AdventOfCode2020

object Day11:
  val directions = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  type Grid = Map[Point, Char]

  def parse(input: Seq[String]): Grid =
    Seq.tabulate(input.head.size, input.size)((x, y) => Point(x, y) -> input(y)(x)).flatten.toMap

  def step(neighbors: (Grid, Point) => Int, threshold: Int)(grid: Grid): Grid = grid.map {
    case (point, '.') => point -> '.'
    case (point, 'L') => point -> (if neighbors(grid, point) > 0 then 'L' else '#')
    case (point, '#') => point -> (if neighbors(grid, point) > threshold then 'L' else '#')
  }

  def simulate(input: Seq[String], step: Grid => Grid): Int =
    val grid = parse(input)
    val (_, current) = Iterator
      .iterate((grid, step(grid)))((_, current) => (current, step(current)))
      .dropWhile((previous, current) => previous != current)
      .next()
    current.values.count(_ == '#')

  def part1(input: Seq[String]): Int =
    def neighbors(grid: Grid, point: Point): Int = directions.count { (dx, dy) =>
      grid.get(point.updated(dx, dy)).exists(_ == '#')
    }
    simulate(input, step(neighbors, 3))

  def part2(input: Seq[String]): Int =
    def neighbors(grid: Grid, point: Point): Int = directions.count { (dx, dy) =>
      val points = Iterator.iterate(point)(_.updated(dx, dy)).drop(1).takeWhile(grid.contains)
      points.map(grid).find(_ != '.').exists(_ == '#')
    }
    simulate(input, step(neighbors, 4))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day11.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
