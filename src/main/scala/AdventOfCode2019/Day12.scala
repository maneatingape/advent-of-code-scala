package AdventOfCode2019

import scala.annotation.tailrec

object Day12:
  case class Point(x: Int, y: Int, z: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
    def -(other: Point): Point = Point(x - other.x, y - other.y, z - other.z)
    def sign: Point = Point(x.sign, y.sign, z.sign)
    def manhattan: Int = x.abs + y.abs + z.abs

  case class Moon(position: Point, velocity: Point):
    def energy: Int = position.manhattan * velocity.manhattan

  def parse(input: Seq[String]): Seq[Moon] =
    val regex = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r
    input.map { case regex(x, y, z) => Moon(Point(x.toInt, y.toInt, z.toInt), Point(0, 0, 0)) }

  def step(moons: Seq[Moon]): Seq[Moon] = moons.map { moon =>
    val velocity = moons.foldLeft(moon.velocity)((total, next) => total + (next.position - moon.position).sign)
    Moon(moon.position + velocity, velocity)
  }

  def gcd(a: Long, b: Long): Long = if a % b == 0 then b else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  def part1(input: Seq[String]): Int = Iterator.iterate(parse(input))(step).drop(1000).next().map(_.energy).sum

  def part2(input: Seq[String]): Long =
    @tailrec
    def halfPeriod(moons: Seq[Moon], dimension: Point => Int, count: Int = 1): Long =
      val next = step(moons)
      if next.map(_.velocity).map(dimension).forall(_ == 0) then count else halfPeriod(next, dimension, count + 1)

    val initial = parse(input)
    val halfPeriodX = halfPeriod(initial, (p: Point) => p.x)
    val halfPeriodY = halfPeriod(initial, (p: Point) => p.y)
    val halfPeriodZ = halfPeriod(initial, (p: Point) => p.z)

    2 * lcm(lcm(halfPeriodX, halfPeriodY), halfPeriodZ)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
