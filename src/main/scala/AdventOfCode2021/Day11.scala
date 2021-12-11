package AdventOfCode2021

object Day11:
  val directions = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  case class Grid(width: Int, height: Int, energy: collection.mutable.Map[Point, Int]):
    def withinBounds(point: Point): Boolean = point.x >= 0 && point.x < width && point.y >= 0 && point.y < height
    def neighbours(point: Point): Seq[Point] = directions.map((dx, dy) => Point(point.x + dx, point.y + dy)).filter(withinBounds)

  case class Point(x: Int, y: Int)

  def parseGrid(input: Seq[String]): Grid =
    val energy = input.map(_.map(_.asDigit))
    val points = Seq.tabulate(energy.head.size, energy.size)((x, y) => Point(x, y) -> energy(y)(x)).flatten
    Grid(energy.head.size, energy.size, collection.mutable.LinkedHashMap.from(points))

  def step(grid: Grid): Int =
    def helper(todo: Seq[Point], flashed: Set[Point]): Set[Point] =
      val (nextTodo, nextFlashed) = todo.foldLeft[(Seq[Point], Set[Point])]((Seq(), flashed)) { case ((todo, flashed), point) =>
        if !flashed.contains(point) then grid.energy(point) += 1
        if grid.energy(point) <= 9 then (todo, flashed)
        else
          grid.energy(point) = 0
          val nextFlashed = flashed + point
          val nextTodo = (todo ++ grid.neighbours(point)).filterNot(flashed.contains)
          (nextTodo, nextFlashed)
      }
      if nextTodo.isEmpty then nextFlashed else helper(nextTodo, nextFlashed)

    helper(grid.energy.keys.toSeq, Set()).size

  def printGrid(grid: Grid): Unit =
    for y <- 0 until grid.height do
      for x <- 0 until grid.width do
        print(grid.energy(Point(x, y)))
      println()
    println()

  def part1(input: Seq[String]): Int =
    val grid = parseGrid(input)
    (1 to 100).map(_ => step(grid)).sum

  def part2(input: Seq[String]): Int =
    val grid = parseGrid(input)
    1 + (1 to Int.MaxValue).indexWhere(_ => step(grid) == grid.width * grid.height)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day11.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
