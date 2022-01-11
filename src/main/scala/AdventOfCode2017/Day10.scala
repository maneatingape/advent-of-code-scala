package AdventOfCode2017

object Day10:
  def knot(lengths: Seq[Int]): Seq[Int] =
    val initial = (Seq.range(0, 256), 0)
    val (data, index) = lengths.zipWithIndex.foldLeft(initial) { case ((data, index), (length, skip)) =>
      val next = data.take(length).reverse ++ data.drop(length)
      val offset = (length + skip) & 0xFF
      (next.drop(offset) ++ next.take(offset), (index - offset) & 0xFF)
    }
    data.drop(index) ++ data.take(index)

  def part1(input: String): Int =
    val lengths = input.split(",").map(_.toInt)
    knot(lengths).take(2).product

  def part2(input: String): String =
    val lengths = input.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)
    val repeated = Seq.fill(64)(lengths).flatten
    knot(repeated).grouped(16).map(_.reduce(_ ^ _)).map(_.formatted("%02x")).mkString

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day10.txt").mkString.trim
    println(part1(data))
    println(part2(data))
