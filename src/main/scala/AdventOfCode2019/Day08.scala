package AdventOfCode2019

object Day08:
  def part1(input: Seq[Int]): Int =
    input.grouped(25 * 6).map(group => (group.count(_ == 0), group.count(_ == 1) * group.count(_ == 2))).minBy(_._1)._2

  def part2(input: Seq[Int]): Seq[Int] = input.grouped(25 * 6).foldLeft(Seq.fill(25 * 6)(2)) { (image, layer) =>
    image.zip(layer).map((previous, current) => if previous == 2 then current else previous)
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day08.txt").mkString.trim.map(_.asDigit)
    println(part1(data))
    val image = part2(data)
    for y <- 0 until 6 do
      println()
      for x <- 0 until 25 do
        print(if image(y * 25 + x) == 1 then '#' else ' ')
