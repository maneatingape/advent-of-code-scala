package AdventOfCode2022

object Day12:
  val orthogonal = Seq(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))

  case class Point(x: Int, y: Int):
    def delta(other: Point): Point = Point(x + other.x, y + other.y)

  def parse(input: Seq[String]): Map[Point, Char] =
    val points = for y <- input.indices; x <- input.head.indices yield Point(x, y) -> input(y)(x)
    points.toMap

  def bfs(grid: Map[Point, Char], end: Char): Int =
    val (start, _) = grid.find((k, v) => v == 'E').get
    val todo = collection.mutable.Queue(start)
    val cost = collection.mutable.Map(start -> 0)

    def height(point: Point): Char = grid(point) match
      case 'S' => 'a'
      case 'E' => 'z'
      case c => c

    while todo.nonEmpty do
      val point = todo.dequeue()
      if grid(point) == end then return cost(point)
      orthogonal.map(point.delta).filter(grid.contains).foreach { next =>
        if height(point) - height(next) <= 1 && !cost.contains(next) then
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
