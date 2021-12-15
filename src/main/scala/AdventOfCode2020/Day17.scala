package AdventOfCode2020

object Day17:
  case class Point(x: Int, y: Int, z: Int, w: Int):
    def updated(delta: Point): Point = Point(x + delta.x, y + delta.y, z + delta.z, w + delta.w)
    def neighbours(directions: Set[Point]): Set[Point] = directions.map(updated)

  def parse(input: Seq[String]): Set[Point] =
    val width = input.head.size
    val height = input.size
    val points = for x <- 0 until width; y <- 0 until height if input(y)(x) == '#' yield Point(x, y, 0, 0)
    points.toSet

  def step(directions: Set[Point])(grid: Set[Point]): Set[Point] =
    def rule(point: Point) =
      val active = grid.contains(point)
      val count = point.neighbours(directions).count(grid.contains)
      count == 3 || (active && count == 4)
    grid.flatMap(point => point.neighbours(directions)).filter(rule)

  def part1(input: Seq[String]): Int =
    val directions = Seq.tabulate(3, 3, 3)((x, y, z) => Point(x - 1, y - 1, z - 1, 0)).flatten.flatten.toSet
    Iterator.iterate(parse(input))(step(directions)).drop(6).next.size

  def part2(input: Seq[String]): Int =
    val directions = Seq.tabulate(3, 3, 3, 3)((x, y, z, w) => Point(x - 1, y - 1, z - 1, w - 1)).flatten.flatten.flatten.toSet
    Iterator.iterate(parse(input))(step(directions)).drop(6).next.size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day17.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
