package AdventOfCode2018

object Day18:
  val adjacent = for y <- -1 to 1; x <- -1 to 1 if !(x == 0 && y == 0) yield (x, y)

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def neighbours: Seq[Point] = adjacent.map(delta)

  type Grid = Map[Point, Char]
  extension (grid: Grid)
    def score: Int = grid.values.count(_ == '|') * grid.values.count(_ == '#')

  def parse(input: Seq[String]): Grid =
    val points = for y <- input.indices; x <- input.head.indices yield Point(x, y) -> input(y)(x)
    points.toMap

  def step(grid: Grid): Grid = grid.map { case (point, value) =>
    def count(c: Char) = point.neighbours.flatMap(grid.get).count(_ == c)
    val next = value match
      case '.' => if count('|') >= 3 then '|' else '.'
      case '|' => if count('#') >= 3 then '#' else '|'
      case '#' => if count('#') >= 1 && count('|') >= 1 then '#' else '.'
    point -> next
  }

  def part1(input: Seq[String]): Int = Iterator.iterate(parse(input))(step).drop(10).next().score

  def part2(input: Seq[String]): Int =
    def helper(grid: Map[Point, Char], index: Int, previous: Seq[Grid]): (Int, Seq[Grid]) =
      val index = previous.lastIndexOf(grid)
      if index != -1 then (index, previous.drop(index))
      else helper(step(grid), index + 1, previous.appended(grid))

    val (offset, cycle) = helper(parse(input), 0, Seq())
    val relativeIndex = ((1000000000L - offset) % cycle.size).toInt
    cycle(relativeIndex).score
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
