package AdventOfCode2021

object Day25:
  case class Point(x: Int, y: Int)
  case class OceanTrench(width: Int, height: Int, seaCucumbers: Map[Point, Char])

  def parse(input: Seq[String]): OceanTrench =
    val width = input.head.size
    val height = input.size
    val points = for x <- 0 until width; y <- 0 until height if input(y)(x) != '.' yield Point(x, y) -> input(y)(x)
    OceanTrench(width, height, points.toMap)

  def step(grid: OceanTrench): OceanTrench =
    val east = grid.seaCucumbers.map { case (point, kind) =>
      val next = Point((point.x + 1) % grid.width, point.y)
      if kind == '>' && !grid.seaCucumbers.contains(next) then next -> kind else point -> kind
    }
    val south = east.map { case (point, kind) =>
      val next = Point(point.x, (point.y + 1) % grid.height)
      if kind == 'v' && !east.contains(next) then next -> kind else point -> kind
    }
    grid.copy(seaCucumbers = south)

  def part1(input: Seq[String]): Int =
    def helper(trench: OceanTrench, steps: Int): Int =
      val next = step(trench)
      if next == trench then steps else helper(next, steps + 1)

    helper(parse(input), 1)
  end part1

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day25.txt").getLines().toSeq
    println(part1(data))
