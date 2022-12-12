package AdventOfCode2022

object Day12:
  val orthogonal = Seq((1, 0), (-1, 0), (0, 1), (0, -1))

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  type Grid = Map[Point, Char]
  extension (grid: Grid)
    def neighbours(point: Point): Seq[Point] = orthogonal.map(point.delta).filter(grid.contains)
    def height(point: Point): Char = grid(point) match
      case 'S' => 'a'
      case 'E' => 'z'
      case c => c

  def parse(input: Seq[String]): Grid =
    val points = for y <- input.indices; x <- input.head.indices yield Point(x, y) -> input(y)(x)
    points.toMap

  def bfs(grid: Grid, end: Char): Int =
    val start = grid.map(_.swap)('E')
    val todo = collection.mutable.Queue(start)
    val cost = collection.mutable.Map(start -> 0)

    while todo.nonEmpty do
      val point = todo.dequeue()
      if grid(point) == end then return cost(point)
      grid.neighbours(point).filterNot(cost.contains).foreach { next =>
        if grid.height(point) - grid.height(next) <= 1 then
          todo.enqueue(next)
          cost(next) = cost(point) + 1
      }

    -1
  end bfs

  def part1(input: Seq[String]): Int = bfs(parse(input), 'S')

  def part2(input: Seq[String]): Int = bfs(parse(input), 'a')

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
