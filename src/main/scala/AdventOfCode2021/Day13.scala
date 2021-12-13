package AdventOfCode2021

object Day13:
  def parse(input: Seq[String]): (Set[(Int, Int)], Seq[(Char, Int)]) =
    val index = input.indexOf("")
    val points = input.take(index).map(_.split(",")).map(a => a(0).toInt -> a(1).toInt).toSet
    val folds = input.drop(index + 1).map(_.split("=")).map(a => a(0).last -> a(1).toInt)
    (points, folds)

  def origami(points: Set[(Int, Int)], folds: Seq[(Char, Int)]): Set[(Int, Int)] = folds.foldLeft(points) {
    case (points, ('x', line)) => points.map((x, y) => (if x < line then x else 2 * line - x, y))
    case (points, ('y', line)) => points.map((x, y) => (x, if y < line then y else 2 * line - y))
  }

  def part1(input: Seq[String]): Int =
    val (points, folds) = parse(input)
    origami(points, folds.take(1)).size

  def part2(input: Seq[String]): Set[(Int, Int)] =
    val (points, folds) = parse(input)
    origami(points, folds)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day13.txt").getLines().toSeq
    println(part1(data))

    val code = part2(data)
    val (width, height) = (code.map(_._1).max, code.map(_._2).max)
    for y <- 0 to height do
      println()
      for x <- 0 to width do
        print(if code.contains((x, y)) then "#" else ".")
