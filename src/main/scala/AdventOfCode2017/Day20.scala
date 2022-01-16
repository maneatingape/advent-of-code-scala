package AdventOfCode2017

object Day20:
  case class Point(x: Int, y: Int, z: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
    def manhattan: Int = x.abs + y.abs + z.abs

  case class Particle(position: Point, velocity: Point, acceleration: Point):
    def step: Particle = Particle(position + velocity + acceleration, velocity + acceleration, acceleration)

  def parse(input: Seq[String]): Seq[Particle] = input.map { line =>
    val Array(sx, sy, sz, vx, vy, vz, ax, ay, az) = line.split("[^-\\d]+").tail.map(_.toInt)
    Particle(Point(sx, sy, sz), Point(vx, vy, vz), Point(ax, ay, az))
  }

  def part1(input: Seq[String]): Int = Iterator.iterate(parse(input))(_.map(_.step))
    .drop(500).next()
    .zipWithIndex.minBy(_._1.position.manhattan)._2

  def part2(input: Seq[String]): Int = Iterator.iterate(parse(input)) { particles =>
      particles.map(_.step).groupBy(_.position).values.filter(_.size == 1).flatten.toSeq
    }
    .drop(500).next()
    .size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day20.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
