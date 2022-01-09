package AdventOfCode2018

object Day23:
  case class Point(x: Int, y: Int, z: Int):
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

  case class Nanobot(position: Point, radius: Int)

  case class Interval(start: Int, end: Int):
    def closest: Int = start.abs.min(end.abs)
    def split: Seq[Interval] =
      val middle = start + (end - start) / 2
      Seq(Interval(start, middle), Interval(middle + 1, end))
    def manhattan(n: Int): Int = if n < start then start - n else if n > end then n - end else 0

  case class Cube(xs: Interval, ys: Interval, zs: Interval):
    def closest: Int = xs.closest + ys.closest + zs.closest
    def split: Seq[Cube] = for nxs <- xs.split; nys <- ys.split; nzs <- zs.split yield Cube(nxs, nys, nzs)
    def manhattan(point: Point): Int = xs.manhattan(point.x) + ys.manhattan(point.y) + zs.manhattan(point.z)

  def parse(input: Seq[String]): Seq[Nanobot] = input.map { line =>
    val Array(x, y, z, r) = line.split("[^-\\d]+").filter(_.nonEmpty).map(_.toInt)
    Nanobot(Point(x, y, z), r)
  }

  def part1(input: Seq[String]): Int =
    val nanobots = parse(input)
    val strongest = nanobots.maxBy(_.radius)
    nanobots.count(_.position.manhattan(strongest.position) <= strongest.radius)

  def part2(input: Seq[String]): Int =
    val nanobots = parse(input)
    def score(cube: Cube) = cube -> nanobots.count(nb => cube.manhattan(nb.position) <= nb.radius)

    def helper(n: Int, cubes: Seq[Cube]): Int =
      if n < 0 then cubes.map(_.closest).min else
        val candidates = cubes.flatMap(_.split).map(score)
        val max = candidates.map(_._2).max
        val remaining = candidates.filter(_._2 == max).map(_._1)
        helper(n - 1, remaining)

    val power = 30
    val initial = 1 << power
    val interval = Interval(-initial, initial - 1)
    helper(power, Seq(Cube(interval, interval, interval)))
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day23.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
