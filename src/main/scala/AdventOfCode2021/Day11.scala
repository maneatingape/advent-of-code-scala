package AdventOfCode2021

object Day11:
  val directions = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  case class Grid(width: Int, height: Int, energy: Map[Point, Int]):
    def withinBounds(point: Point): Boolean = point.x >= 0 && point.x < width && point.y >= 0 && point.y < height
    def neighbours(point: Point): Seq[Point] = directions.map((dx, dy) => Point(point.x + dx, point.y + dy)).filter(withinBounds)
    def updated(point: Point, value: Int) = copy(energy = energy + (point -> value))

  case class Point(x: Int, y: Int)

  def parseGrid(input: Seq[String]): Grid =
    val energy = input.map(_.map(_.asDigit))
    val points = Seq.tabulate(energy.head.size, energy.size)((x, y) => Point(x, y) -> energy(y)(x))
    Grid(energy.head.size, energy.size, points.flatten.toMap)

  def step(grid: Grid): (Grid, Int) =
    def helper(grid: Grid, todo: Seq[Point] = grid.energy.keys.toSeq, flashed: Set[Point] = Set()): (Grid, Int) =
      val (nextGrid, nextTodo, nextFlashed) = todo.foldLeft((grid, Seq.empty[Point], flashed)) { case ((grid, todo, flashed), point) =>
        if flashed.contains(point) then
          (grid, todo, flashed)
        else if grid.energy(point) < 9 then
          (grid.updated(point, grid.energy(point) + 1), todo, flashed)
        else
          val nextTodo = todo ++ grid.neighbours(point).filterNot(flashed.contains)
          (grid.updated(point, 0), nextTodo, flashed + point)
      }
      if nextTodo.isEmpty then (nextGrid, nextFlashed.size) else helper(nextGrid, nextTodo, nextFlashed)
    helper(grid, grid.energy.keys.toSeq, Set())
  end step

  def printGrid(grid: Grid): Unit =
    for y <- 0 until grid.height do
      for x <- 0 until grid.width do
        print(grid.energy(Point(x, y)))
      println()
    println()

  def part1(input: Seq[String]): Int =
    val grid = parseGrid(input)
    val (_, total) = (1 to 100).foldLeft((grid, 0)) { case ((grid, total), _) =>
      val (nextGrid, flashes) = step(grid)
      (nextGrid, total + flashes)
    }
    total

  def part2(input: Seq[String]): Int =
    var grid = parseGrid(input)
    def countFlashes: Int = {
      val (nextGrid, flashes) = step(grid)
      grid = nextGrid
      flashes
    }
    1 + Iterator.continually(countFlashes).indexWhere(_ == grid.width * grid.height)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day11.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
