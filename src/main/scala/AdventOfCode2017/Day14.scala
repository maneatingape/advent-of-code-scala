package AdventOfCode2017

object Day14:
  val orthogonal = Set((-1, 0), (1, 0), (0, -1), (0, 1))

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def neighbours: Set[Point] = orthogonal.map(delta)

  def knot(input: String): Seq[Int] =
    val lengths = input.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)
    val repeated = Seq.fill(64)(lengths).flatten
    val initial = (Seq.range(0, 256), 0)
    val (data, index) = repeated.zipWithIndex.foldLeft(initial) { case ((data, index), (length, skip)) =>
      val next = data.take(length).reverse ++ data.drop(length)
      val offset = (length + skip) & 0xFF
      (next.drop(offset) ++ next.take(offset), (index - offset) & 0xFF)
    }
    (data.drop(index) ++ data.take(index)).grouped(16).map(_.reduce(_ ^ _)).toSeq

  def binary(input: String): Seq[String] =
    (0 to 127).map(index => knot(s"$input-$index").map(s => "%08d".format(s.toBinaryString.toInt)).mkString)

  def part1(input: String): Int = binary(input).map(_.count(_ == '1')).sum

  def part2(input: String): Int =
    val points = binary(input).zipWithIndex.flatMap { case (row, y) =>
      for x <- 0 to 127 if row(x) == '1' yield Point(x, y)
    }
    val cliques =  points.foldLeft(Set.empty[Set[Point]]) { (groups, point) =>
      val (other, linked) = groups.partition(_.intersect(point.neighbours).size == 0)
      other + (linked.flatten + point)
    }
    cliques.size

  def main(args: Array[String]): Unit =
    val data = "xlqgujun"
    println(part1(data))
    println(part2(data))
