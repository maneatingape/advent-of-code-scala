package AdventOfCode2018

object Day03:
  case class Point(x: Int, y: Int)

  def parse(input: Seq[String]): (Seq[Seq[Point]], Map[Point, Int]) =
    val regex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
    val claims = input
      .map { case regex(groups*) => groups.map(_.toInt) }
      .map { case Seq(id, left, top, width, height) =>
        for x <- left until left + width; y <- top until top + height yield Point(x, y)
      }
    val overlaps = claims.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    (claims, overlaps)
  end parse

  def part1(input: Seq[String]): Int =
    val (_, overlaps) = parse(input)
    overlaps.map(_._2).count(_ > 1)

  def part2(input: Seq[String]): Int =
    val (claims, overlaps) = parse(input)
    1 + claims.indexWhere(claim => claim.forall(point => overlaps(point) == 1))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day03.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
