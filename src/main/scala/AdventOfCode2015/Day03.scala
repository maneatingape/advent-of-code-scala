package AdventOfCode2015

object Day03:
  val directions = Map('^' -> (0, -1), 'v' -> (0, 1), '<' -> (-1, 0), '>' -> (1, 0))

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int) = Point(x + dx, y + dy)

  def deliver(input: Seq[Char]): Seq[Point] =
    input.scanLeft(Point(0, 0))((location, next) => location.delta.tupled(directions(next)))

  def part1(input: String): Int = deliver(input).distinct.size

  def part2(input: String): Int =
    val (santa, robot) = input.zipWithIndex.partitionMap((c, i) => if i % 2 == 0 then Left(c) else Right(c))
    (deliver(santa) ++ deliver(robot)).distinct.size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day03.txt").mkString.trim
    println(part1(data))
    println(part2(data))
