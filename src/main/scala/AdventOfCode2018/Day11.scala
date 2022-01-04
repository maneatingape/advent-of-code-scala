package AdventOfCode2018

object Day11:
  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  def fuelCells(serialNumber: Int): Map[Point, Int] =
    val points = for x <- 1 to 300; y <- 1 to 300 yield
      val powerLevel = (((x + 10) * y + serialNumber) * (x + 10) / 100) % 10 - 5
      Point(x, y) -> powerLevel
    points.toMap

  def summedAreaTable(fuelCells: Map[Point, Int]): Map[Point, Int] =
    val points = for x <- 1 to 300; y <- 1 to 300 yield Point(x, y)
    points.foldLeft(Map.empty[Point, Int].withDefaultValue(0)) { (sat, point) =>
      val area = fuelCells(point) + sat(point.delta(-1, 0)) + sat(point.delta(0, -1)) - sat(point.delta(-1, -1))
      sat.updated(point, area)
    }

  def powerGrid(sat: Map[Point, Int], size: Int): Seq[(Int, Point, Int)] =
    val points = for x <- size to 300; y <- size to 300 yield Point(x, y)
    points.map { point =>
      val area = sat(point) - sat(point.delta(-size, 0)) - sat(point.delta(0, -size)) + sat(point.delta(-size, -size))
      (size, point, area)
    }

  def part1(serialNumber: Int): String =
    val sat = summedAreaTable(fuelCells(serialNumber))
    val candidates = powerGrid(sat, 3)
    val (size, point, area) = candidates.maxBy(_._3)
    Seq(point.x - size + 1, point.y - size + 1).mkString(",")

  def part2(serialNumber: Int): String =
    val sat = summedAreaTable(fuelCells(serialNumber))
    val candidates = for size <- 1 to 300 yield powerGrid(sat, size)
    val (size, point, area) = candidates.flatten.maxBy(_._3)
    Seq(point.x - size + 1, point.y - size + 1, size).mkString(",")

  def main(args: Array[String]): Unit =
    val serialNumber = 9005
    println(part1(serialNumber))
    println(part2(serialNumber))
