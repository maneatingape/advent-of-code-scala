package AdventOfCode2022

object Day09:
  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def delta(other: Point): Point = Point((x - other.x).sign, (y - other.y).sign)
    def touching(other: Point): Boolean = (x - other.x).abs <= 1 && (y - other.y).abs <= 1

  def parse(input: Seq[String]): Seq[Point] =
    val orthogonal = Map("R" -> Point(1, 0), "L" -> Point(-1, 0), "D" -> Point(0, 1), "U" -> Point(0, -1))
    input.flatMap { line =>
      val Array(direction, amount) = line.split(" ")
      Seq.fill(amount.toInt)(orthogonal(direction))
    }

  def simulate(input: Seq[String], size: Int): Int =
    val steps = parse(input)
    val initial = Seq.fill(size)(Point(0, 0))
    steps.scanLeft(initial) { case (rope, step) =>
      val next = rope.head + step
      rope.tail.scanLeft(next) { (previous, current) =>
        if (previous.touching(current)) current else current + previous.delta(current)
      }
    }
    .map(_.last).distinct.size

  def part1(input: Seq[String]): Int = simulate(input, 2)

  def part2(input: Seq[String]): Int = simulate(input, 10)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day09.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
