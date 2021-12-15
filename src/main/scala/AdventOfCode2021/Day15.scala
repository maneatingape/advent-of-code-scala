package AdventOfCode2021

object Day15:
  val directions = Seq((1, 0), (-1, 0), (0, -1), (0, 1))

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  type Grid = Map[Point, Int]
  extension (grid: Grid)
    def neighbours(point: Point): Seq[Point] = directions.map(point.updated).filter(grid.contains)

  def parse(input: Seq[String]): Grid =
    Seq.tabulate(input.head.size, input.size)((x, y) => Point(x, y) -> input(y)(x).asDigit).flatten.toMap

  def path(grid: Grid): Int =
    val (start, end) = (Point(0, 0), grid.keys.maxBy(p => p.x * p.y))
    val risk = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.Queue(start)

    while todo.nonEmpty do
      val point = todo.dequeue()
      grid.neighbours(point).foreach { next =>
        if !risk.contains(next) || risk(point) + grid(next) < risk(next) then
          risk(next) = risk(point) + grid(next)
          todo.enqueue(next)
      }

    risk(end)
  end path

  def expand(grid: Grid): Grid =
    val end = grid.keys.maxBy(p => p.x * p.y)
    val (width, height) = (end.x + 1, end.y + 1)
    List.tabulate(5, 5) { (x, y) =>
      grid.toSeq.map { (point, value) =>
        Point(x * width + point.x, y * height + point.y) -> (1 + (value - 1 + x + y) % 9)
      }
    }
    .flatten.flatten.toMap

  def part1(input: Seq[String]): Int = path(parse(input))

  def part2(input: Seq[String]): Int = path(expand(parse(input)))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day15.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
