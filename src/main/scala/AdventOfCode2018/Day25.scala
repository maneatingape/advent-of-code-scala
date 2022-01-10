package AdventOfCode2018

object Day25:
  case class Point(x: Int, y: Int, z: Int, w: Int):
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs + (w - other.w).abs

  def part1(input: Seq[String]): Int = input
    .map { _.split(",").map(_.toInt) }
    .map { case Array(x, y, z, w) => Point(x, y, w, z) }
    .foldLeft(Set.empty[Set[Point]]) { (constellations, point) =>
      val (near, far) = constellations.partition(_.exists(_.manhattan(point) <= 3))
      far + (near.flatten + point)
    }
    .size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day25.txt").getLines().toSeq
    println(part1(data))
