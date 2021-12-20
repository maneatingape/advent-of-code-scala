package AdventOfCode2021

object Day20:
  val directions = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  extension (c: Char) def toDigit: Int = if c == '#' then 1 else 0
  extension (seq: Seq[Int]) def binary: Int = Integer.parseInt(seq.mkString, 2)

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def neighbours: Seq[Point] = directions.map(updated)

  case class Grid(min: Int, max: Int, pixels: Map[Point, Int]):
    def index(point: Point, default: Int): Int = point.neighbours.map(pixels.getOrElse(_, default)).binary
    def lit: Int = pixels.values.sum

  def parse(input: Seq[String]): (Seq[Int], Grid) =
    val algorithm = input.head.map(toDigit)
    val data = input.drop(2)
    val pixels = Seq.tabulate(data.head.size, data.size)((x, y) => Point(x, y) -> data(y)(x).toDigit)
    (algorithm, Grid(-1, data.size, pixels.flatten.toMap))

  def step(algorithm: Seq[Int])(default: Int, grid: Grid): (Int, Grid) =
    val pixels = for
      y <- grid.min to grid.max
      x <- grid.min to grid.max
    yield
      val index = grid.index(Point(x, y), default)
      Point(x, y) -> algorithm(index)

    val nextDefault = if default == 0 then algorithm.head else algorithm.last
    val nextGrid = Grid(grid.min - 1, grid.max + 1, pixels.toMap)
    (nextDefault, nextGrid)
  end step

  def enhance(input: Seq[String], steps: Int): Int =
    val (algorithm, grid) = parse(input)
    val (_, enhanced) = Iterator.iterate((0, grid))(step(algorithm)).drop(steps).next()
    enhanced.lit

  def part1(input: Seq[String]): Int = enhance(input, 2)

  def part2(input: Seq[String]): Int = enhance(input, 50)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day20.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
