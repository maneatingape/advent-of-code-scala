package AdventOfCode2022

object Day24:
  val moves = Seq(Point(0, 0), Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def neighbours: Seq[Point] = moves.map(_ + this)

  def parse(input: Seq[String]): (Point, Point, (Set[Point], Point, Int) => Int) =
    val width = input.head.size - 2
    val height = input.size - 2

    def mod(a: Int, m: Int): Int =
      val remainder = (a - 1) % m
      if remainder < 0 then remainder + m + 1 else remainder + 1

    def valid(time: Int)(point: Point): Boolean =
      val Point(x, y) = point
      input.indices.contains(y)
      && input(y)(x) != '#'
      && input(y)(mod(x + time, width)) != '<'
      && input(y)(mod(x - time, width)) != '>'
      && input(mod(y + time, height))(x) != '^'
      && input(mod(y - time, height))(x) != 'v'

    def simulate(points: Set[Point], end: Point, time: Int): Int =
      if points.contains(end) then time else
        val next = points.flatMap(_.neighbours).filter(valid(time + 1))
        simulate(next, end, time + 1)

    (Point(1, 0), Point(width, height + 1), simulate)
  end parse

  def part1(input: Seq[String]): Int =
    val (start, end, simulate) = parse(input)
    simulate(Set(start), end, 0)

  def part2(input: Seq[String]): Int =
    val (start, end, simulate) = parse(input)
    val time1 = simulate(Set(start), end, 0)
    val time2 = simulate(Set(end), start, time1)
    simulate(Set(start), end, time2)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day24.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
