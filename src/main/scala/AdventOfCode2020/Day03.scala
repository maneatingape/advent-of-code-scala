package AdventOfCode2020

object Day03:
  def parse(input: Seq[String]): Seq[Seq[Boolean]] = input.map(_.map(_ == '#'))

  def slope(trees: Seq[Seq[Boolean]], right: Int, down: Int): Int =
    val ys = (0 until trees.size by down).zipWithIndex.drop(1)
    ys.count((y, index) => trees(y)((right * index) % trees.head.size))

  def risk(input: Seq[String], slopes: Seq[(Int, Int)]): Long =
    val trees = parse(input)
    slopes.map(slope(trees, _, _)).map(_.toLong).product

  def part1(input: Seq[String]): Long = risk(input, Seq((3, 1)))

  def part2(input: Seq[String]): Long = risk(input, Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day03.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
